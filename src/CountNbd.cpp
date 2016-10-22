// Serial routines (Kd)
#include <Rcpp.h>
using namespace Rcpp;

// Kd
// [[Rcpp::export]]
void DistKd(SEXP Rx, SEXP Ry, SEXP RPointWeight, SEXP RWeight, SEXP RDist, SEXP RIsReferenceType, SEXP RIsNeighborType) {
// Fill a distance vector between each (reference point, neighbor point) pair for Kd
// If weighted, also fill a similar vector with the product of point Weights

  // x, y coordinates of points
  NumericVector x(Rx);
  NumericVector y(Ry);
  // Point weights
  NumericVector PointWeight(RPointWeight);
  // A vector for weights of point pairs
  NumericVector Weight(RWeight);
  // A vector for distances between point pairs
  NumericVector Dist(RDist);
  // Boolean vectors describing reference and neighbor points
  IntegerVector IsReferenceType(RIsReferenceType);
  IntegerVector IsNeighborType(RIsNeighborType);
  
  // Kd is weighted if a vector has been passed by R. Else, a single numeric value has been passed.
  bool Weighted = (Weight.length() > 1);

  int d=0;
  double dx, dy;
  for (int i=0; i < (x.length()-1); i++) {
    // Point j is a neighbor of i
    for (int j=i+1; j < x.length(); j++) {
      // i and j must be reference and neighbor
      if ((IsReferenceType[i] & IsNeighborType[j]) | (IsReferenceType[j] & IsNeighborType[i])) {
        // Calculate distance
        dx = x[i]-x[j];
        dy = y[i]-y[j];
        Dist[d] = sqrt(dx*dx + dy*dy);
        if (Weighted) {
          // if weighted, calculate the product of weights
          Weight[d] = PointWeight[i]*PointWeight[j];
        }
        d++;
      }
    }
  }
}

// Kd, approximated
// [[Rcpp::export]]
void CountNbdKd(SEXP Rr, SEXP Rx, SEXP Ry, SEXP RWeight, SEXP RNbd, SEXP RIsReferenceType, SEXP RIsNeighborType) {
// Count the number of neighbors around each point for Kd, same as CountNbd but 
   // consider neighbors of interest only
   // do no attribute neighbors to each point, mix them
// Only ReferenceType points are considered. 
// The weights of NeighborType points are counted.

  //Distances
  NumericVector r(Rr);
  // x, y coordinates of points
  NumericVector x(Rx);
  NumericVector y(Ry);
  // Point weights
  NumericVector Weight(RWeight);
  // Matrix (single row) counting the number of neighbors. Modified by this routine.
  NumericMatrix Nbd(RNbd);
  // Boolean vectors describing reference and neighbor points
  IntegerVector IsReferenceType(RIsReferenceType);
  IntegerVector IsNeighborType(RIsNeighborType);

  double Distance2, dx, dy;
  double Nr = r.length();
  NumericVector r2 = r*r;
  int k; 
  
  for (int i=0; i < (x.length()-1); i++) {
    // Consider reference type points
    if (IsReferenceType[i]) {
      // Point j is a neighbor of i. No neighbor is ignored.
      for (int j=i+1; j < x.length(); j++) {
        // Calculate squared distance
        dx = x[i]-x[j];
        dy = y[i]-y[j];
        Distance2 = dx*dx + dy*dy;
        if (Distance2 <= r2[Nr-1]) {
          // Find the column of the matrix corresponding to the distance
          k = 0; 
          while (Distance2 > r2[k]) {
            k++;
          }
        } else {
          // Extra column for pairs far away
          k = Nr;
        }
        // The neighbor is a point of interest
        if (IsNeighborType[j]) {
          Nbd(0, k) += Weight[i]*Weight[j];
        }
        // j is a reference point
        if (IsReferenceType[j]) {
          // i is a point of interest around j
          if (IsNeighborType[i]) {
            Nbd(0, k) += Weight[i]*Weight[j];
          }
        }        
      }
    } else {
      // Point i is not a reference point
      for (int j=i+1; j < x.length(); j++) {
        // If point j is a reference point, it may be in its neighborhood
        if (IsReferenceType[j]) {
          // Calculate squared distance
          dx = x[i]-x[j];
          dy = y[i]-y[j];
          Distance2 = dx*dx + dy*dy;
          if (Distance2 <= r2[Nr-1]) {
            // Find the column of the matrix corresponding to the distance
            k = 0; 
            while (Distance2 > r2[k]) {
              k++;
            }
          } else {
            // Extra column for pairs far away
            k = Nr;
          }
          // i is a point of interest around j
          if (IsNeighborType[i]) {
            Nbd(0, k) += Weight[i]*Weight[j];
          }
        }
      }
    }
  }
}



// Parallel routines
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

// M and approximated m, applied to wmppp
struct CountNbdWrkr : public Worker
{
  // source vectors
  const RVector<double> r2;
  const RVector<double> Rx;
  const RVector<double> Ry;
  const RVector<double> RWeight;
  const RVector<int> RIsReferenceType;
  const RVector<int> RIsNeighborType;
  
  // destination matrix
  RMatrix<double> RNbd;
  
  // constructor
  CountNbdWrkr(const NumericVector r2, 
               const NumericVector x, const NumericVector y, const NumericVector Weight, 
               const LogicalVector IsReferenceType, const LogicalVector IsNeighborType,
               NumericMatrix Nbd) 
    : r2(r2), Rx(x), Ry(y), RWeight(Weight), RIsReferenceType(IsReferenceType), RIsNeighborType(IsNeighborType), RNbd(Nbd) {}
  
  // count neighbors
  void operator()(std::size_t begin, std::size_t end) {
    double Distance2, dx, dy;
    double Nr = r2.length();
    unsigned int k;
    
    for (unsigned int i=begin; i < end; i++) {
      // Consider reference type points
      if (RIsReferenceType[i]) {
        // Point j is a neighbor of i. No neighbor is ignored.
        for (unsigned int j=0; j < RNbd.nrow(); j++) {
          if (i != j) {
            // Calculate squared distance
            dx = Rx[i]-Rx[j];
            dy = Ry[i]-Ry[j];
            Distance2 = dx*dx + dy*dy;
            // Ignore point j if it is too far from point i
            if (Distance2 <= r2[Nr-1]) {
              // Find the column of the matrix corresponding to the distance
              k = 0; 
              while (Distance2 > r2[k]) {
                k++;
              }
              // Add j's weight to i's neighborhood
              RNbd(i, Nr+k) += RWeight[j];
              // The neighbor is a point of interest
              if (RIsNeighborType[j]) {
                RNbd(i, k) += RWeight[j];
              }
            }
          }
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelCountNbd(NumericVector r, 
                               NumericVector x, NumericVector y, NumericVector Weight, 
                               LogicalVector IsReferenceType, LogicalVector IsNeighborType) {
  
  // allocate the output matrix
  NumericMatrix Nbd(x.length(), 2*r.length());
  
  // CountNbd functor
  CountNbdWrkr countNbdWrkr(r*r, x, y, Weight, IsReferenceType, IsNeighborType, Nbd);
  
  // call parallelFor to do the work
  parallelFor(0, x.length(), countNbdWrkr);
  
  // return the output matrix
  return Nbd;
}



// M applied to Dtable and approximated m,
struct CountNbdDtWrkr : public Worker
{
  // source vectors, distances are not squared
  const RVector<double> r1;
  const RMatrix<double> RDmatrix;
  const RVector<double> RWeight;
  const RVector<int> RIsReferenceType;
  const RVector<int> RIsNeighborType;
  
  // destination matrix
  RMatrix<double> RNbd;
  
  // constructor
  CountNbdDtWrkr(const NumericVector r1, 
               const NumericMatrix Dmatrix, const NumericVector Weight, 
               const LogicalVector IsReferenceType, const LogicalVector IsNeighborType,
               NumericMatrix Nbd) 
    : r1(r1), RDmatrix(Dmatrix), RWeight(Weight), RIsReferenceType(IsReferenceType), RIsNeighborType(IsNeighborType), RNbd(Nbd) {}
  
  // count neighbors
  void operator()(std::size_t begin, std::size_t end) {
    double Nr = r1.length();
    unsigned int k;
    
    for (unsigned int i=begin; i < end; i++) {
      // Consider reference type points
      if (RIsReferenceType[i]) {
        // Point j is a neighbor of i. No neighbor is ignored.
        for (unsigned int j=0; j < RNbd.nrow(); j++) {
          if (i != j) {
            // Ignore point j if it is too far from point i
            if (RDmatrix(i, j) <= r1[Nr-1]) {
              // Find the column of the matrix corresponding to the distance
              k = 0; 
              while (RDmatrix(i, j) > r1[k]) {
                k++;
              }
              // Add j's weight to i's neighborhood
              RNbd(i, Nr+k) += RWeight[j];
              // The neighbor is a point of interest
              if (RIsNeighborType[j]) {
                RNbd(i, k) += RWeight[j];
              }
            }
          }
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelCountNbdDt(NumericVector r, 
                                 NumericMatrix Dmatrix, NumericVector Weight, 
                                 LogicalVector IsReferenceType, LogicalVector IsNeighborType) {
  
  // allocate the output matrix
  NumericMatrix Nbd(Weight.length(), 2*r.length());
  
  // CountNbd functor, distances are not squared
  CountNbdDtWrkr countNbdDtWrkr(r, Dmatrix, Weight, IsReferenceType, IsNeighborType, Nbd);
  
  // call parallelFor to do the work
  parallelFor(0, Weight.length(), countNbdDtWrkr);
  
  // return the output matrix
  return Nbd;
}



// M and approximated m, case-control, applied to wmppp
struct CountNbdCCWrkr : public Worker
{
  // source vectors
  const RVector<double> r2;
  const RVector<double> Rx;
  const RVector<double> Ry;
  const RVector<double> RWeight;
  const RVector<int> RIsReferenceType;
  const RVector<int> RIsNeighborType;
  
  // destination matrix
  RMatrix<double> RNbd;
  
  // constructor
  CountNbdCCWrkr(const NumericVector r2, 
               const NumericVector x, const NumericVector y, const NumericVector Weight, 
               const LogicalVector IsReferenceType, const LogicalVector IsNeighborType,
               NumericMatrix Nbd) 
    : r2(r2), Rx(x), Ry(y), RWeight(Weight), RIsReferenceType(IsReferenceType), RIsNeighborType(IsNeighborType), RNbd(Nbd) {}
  
  // count neighbors
  void operator()(std::size_t begin, std::size_t end) {
    double Distance2, dx, dy;
    double Nr = r2.length();
    unsigned int k;
    
    for (unsigned int i=begin; i < end; i++) {
      // Consider cases
      if (RIsReferenceType[i]) {
        // Point j is a neighbor of i
        for (unsigned int j=0; j < RNbd.nrow(); j++) {
          if (i != j) {
            // Ignore point j if it is neither a case nor a control
            if (RIsNeighborType[j] || RIsReferenceType[j]) {
              // Calculate squared distance
              dx = Rx[i]-Rx[j];
              dy = Ry[i]-Ry[j];
              Distance2 = dx*dx + dy*dy;
              // Ignore point j if it is too far from point i
              if (Distance2 <= r2[Nr-1]) {
                // Find the column of the matrix corresponding to the distance
                k = 0; 
                while (Distance2 > r2[k]) {
                  k++;
                }
                // The neighbor is a control: add j's weight to i's neighborhood
                if (RIsNeighborType[j]) {
                  RNbd(i, Nr+k) += RWeight[j];
                }
                // The neighbor is a case: add it to i's neighborhood
                if (RIsReferenceType[j]) {
                  RNbd(i, k) += RWeight[j];
                }
              }
            }
          }
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelCountNbdCC(NumericVector r, 
                                NumericVector x, NumericVector y, NumericVector Weight, 
                                LogicalVector IsReferenceType, LogicalVector IsNeighborType) {
  
  // allocate the output matrix
  NumericMatrix Nbd(x.length(), 2*r.length());
  
  // CountNbd functor
  CountNbdCCWrkr countNbdCCWrkr(r*r, x, y, Weight, IsReferenceType, IsNeighborType, Nbd);
  
  // call parallelFor to do the work
  parallelFor(0, x.length(), countNbdCCWrkr);
  
  // return the output matrix
  return Nbd;
}


// M and approximated m, case-control, applied to Dtable
struct CountNbdDtCCWrkr : public Worker
{
  // source vectors, distances are not squared
  const RVector<double> r1;
  const RMatrix<double> RDmatrix;
  const RVector<double> RWeight;
  const RVector<int> RIsReferenceType;
  const RVector<int> RIsNeighborType;
  
  // destination matrix
  RMatrix<double> RNbd;
  
  // constructor
  CountNbdDtCCWrkr(const NumericVector r1, 
                  const NumericMatrix Dmatrix, const NumericVector Weight, 
                  const LogicalVector IsReferenceType, const LogicalVector IsNeighborType,
                  NumericMatrix Nbd) 
    : r1(r1), RDmatrix(Dmatrix), RWeight(Weight), RIsReferenceType(IsReferenceType), RIsNeighborType(IsNeighborType), RNbd(Nbd) {}
  
  // count neighbors
  void operator()(std::size_t begin, std::size_t end) {
    double Nr = r1.length();
    unsigned int k;
    
    for (unsigned int i=begin; i < end; i++) {
      // Consider cases
      if (RIsReferenceType[i]) {
        // Point j is a neighbor of i.
        for (unsigned int j=0; j < RNbd.nrow(); j++) {
          if (i != j) {
            // Ignore point j if it is neither a case nor a control
            if (RIsNeighborType[j] || RIsReferenceType[j]) {
              // Ignore point j if it is too far from point i
              if (RDmatrix(i, j) <= r1[Nr-1]) {
                // Find the column of the matrix corresponding to the distance
                k = 0; 
                while (RDmatrix(i, j) > r1[k]) {
                  k++;
                }
                // The neighbor is a control: add j's weight to i's neighborhood
                if (RIsNeighborType[j]) {
                  RNbd(i, Nr+k) += RWeight[j];
                }
                // The neighbor is a case: add it to i's neighborhood
                if (RIsReferenceType[j]) {
                  RNbd(i, k) += RWeight[j];
                }
              }
            }
          }
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelCountNbdDtCC(NumericVector r, 
                                  NumericMatrix Dmatrix, NumericVector Weight, 
                                  LogicalVector IsReferenceType, LogicalVector IsNeighborType) {
  
  // allocate the output matrix
  NumericMatrix Nbd(Weight.length(), 2*r.length());
  
  // CountNbd functor, distances are not squared
  CountNbdDtCCWrkr countNbdDtCCWrkr(r, Dmatrix, Weight, IsReferenceType, IsNeighborType, Nbd);
  
  // call parallelFor to do the work
  parallelFor(0, Weight.length(), countNbdDtCCWrkr);
  
  // return the output matrix
  return Nbd;
}



// m
struct CountNbdmWrkr : public Worker
{
  // source vectors
  const RVector<double> Rx;
  const RVector<double> Ry;
  const RVector<int> RReferencePoints;
  
  // destination matrix
  RMatrix<double> RNbd;
  
  // constructor
  CountNbdmWrkr(const NumericVector x, const NumericVector y,
               const IntegerVector ReferencePoints,
               NumericMatrix Nbd) 
    : Rx(x), Ry(y), RReferencePoints(ReferencePoints), RNbd(Nbd) {}
  
  // count neighbors
  void operator()(std::size_t begin, std::size_t end) {
    double dx, dy;
    unsigned int k;
    
    for (unsigned int i=begin; i < end; i++) {
      // Get the index of the reference point
      k = RReferencePoints[i];
      // Point j is a neighbor of k. No neighbor is ignored.
      for (unsigned int j=0; j < RNbd.ncol(); j++) {
        if (k == j) {
          // Store a negative value to transform it easily into NA in R
          RNbd(i, j) = -1;
        } else {
          // Calculate the distance and store it
          dx = Rx[k]-Rx[j];
          dy = Ry[k]-Ry[j];
          RNbd(i, j) = sqrt(dx*dx + dy*dy);
        }
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix parallelCountNbdm(NumericVector x, NumericVector y,
                               IntegerVector ReferencePoints) {
  
  // allocate the output matrix
  NumericMatrix Nbd(ReferencePoints.length(), x.length());
  
  // CountNbd functor
  CountNbdmWrkr countNbdmWrkr(x, y, ReferencePoints, Nbd);
  
  // call parallelFor to do the work
  parallelFor(0, ReferencePoints.length(), countNbdmWrkr);
  
  // return the output matrix
  return Nbd;
}
