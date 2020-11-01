Kdhat <-
function(X, r = NULL, ReferenceType, NeighborType = ReferenceType, Weighted = FALSE,
         Original = TRUE, Approximate = ifelse(X$n < 10000, 0, 1), Adjust = 1,
         MaxRange = "ThirdW", StartFromMinR = FALSE, CheckArguments = TRUE) {
  
  if (CheckArguments) {
    CheckdbmssArguments()
  }
  
  # The default number of values used by density is 512. It may be changed if needed.
  n <- 512
  
  # Vectors to recognize point types
  if (ReferenceType == "") {
    # All points (reference value as the center of the confidence interval)
    IsReferenceType <- IsNeighborType <- rep(TRUE, X$n)
    Y <- X 
  } else {
    # Current use
    IsReferenceType <- X$marks$PointType==ReferenceType
    IsNeighborType <- X$marks$PointType==NeighborType    
    # Eliminate useless points
    if (inherits(X, "Dtable")) {
      # Dtable case
      Y <- Dtable(X$Dmatrix[IsReferenceType | IsNeighborType, IsReferenceType | IsNeighborType], 
                  PointType = X$marks$PointType[IsReferenceType | IsNeighborType], 
                  PointWeight = X$marks$PointWeight[IsReferenceType | IsNeighborType])
      # Set the diagonal to NA to prepare density estimation without those zeros
      diag(Y$Dmatrix) <- NA
    } else {
      # wmppp case
      Y <- X[IsReferenceType | IsNeighborType]
    }
    # Update for Y
    IsReferenceType <- Y$marks$PointType==ReferenceType
    IsNeighborType <- Y$marks$PointType==NeighborType
  }

  # Roughly estimate max distances
  if(is.null(r)) {
    Diameter <- 0
    # Estimate max distance in the point set
    if (inherits(X, "Dtable")) {
      # Dtable case
      Diameter <- max(X$Dmatrix)
    } else {
      # wmppp case, approximate rmax by the diameter of the window
      Diameter <- diameter(X$win)
    }
    # Set rmax to window /2, /3 or /4. DO2005 is ignored at this stage.
    rmax <- switch(MaxRange,
                   HalfW = Diameter/2,
                   ThirdW =  Diameter/3,
                   QuarterW = Diameter/4)
    if(is.null(rmax)) rmax <- Diameter/3
  } else {
    rmax <- max(r)
  }
  
  if (Approximate & !inherits(X, "Dtable")) {
    # Round distances to save memory
    # Prepare steps so that 1024*Approximate steps are between 0 and rmax.
    # Pairs further than 2*rmax apart will be stored in an extra element.
    rseq <- seq(from=0, to=rmax*2, length.out=2048*Approximate)
    # Number of distances
    Nr <- length(rseq)
    # Prepare a matrix: single line, one value for each distance + 1 extra for pairs far away.
    NeighborWeight <- matrix(0.0, nrow=1, ncol=Nr+1)
    # Weights
    if (Weighted) {
      Weight <- Y$marks$PointWeight
    } else {
      Weight <- rep(1, Y$n)
    }
    
    # Call C routine to fill NeighborWeights
    CountNbdKd(rseq, Y$x, Y$y, Weight, NeighborWeight, IsReferenceType, IsNeighborType)
    
    # Adjust distances: values are the centers of intervals.
    # rseq becomes a vector (it was a 1-row matrix)
    rseq <- c(0, (rseq[2:Nr]+rseq[1:Nr-1])/2)
    
    # Estimate the bandwith according to adjust if requested.
    # Distances are the values of rseq corresponding to at least a pair of points i.e. NeighborWeight > 0
    # Ignore the last value of NeighborWeight which contains far neighbors
    if (Original) {
      bw <- stats::bw.nrd0(rseq[NeighborWeight[-length(NeighborWeight)]>0]) * Adjust
    } else {
      bw <- stats::bw.SJ(rseq[NeighborWeight[-length(NeighborWeight)]>0]) * Adjust
    }
    
    # Add a last value to rseq equal to 2*rmax+4bw (i.e. ignored by the estimation of density at rmax) for far neighbors
    rseq <- c(rseq, rmax*2 + 4*bw)
    # Estimated density is false above 2rmax-4bw, but it will be censored at rmax.
    # The total mass is correct. It is needed for normalization after mirroring.

    # Prepare reflection. Distances below 4bw are mirrored around 0. The first one is 0: ignore it. Code adapted from GoFKernel::density.reflected
    Reflected <- which(rseq <= 4*bw)[-1]
    rseq <- c(rseq, -rseq[Reflected])
    NeighborWeight <- c(NeighborWeight, NeighborWeight[Reflected])
    # Sum of weights to normalize them to avoid warning during density estimation
    SumNeighborWeight <- sum(NeighborWeight)
    # Estimate density and the density of the mirrored values (below 0) to renormalize later
    Density <- stats::density(rseq, weight=NeighborWeight/SumNeighborWeight, from=0, to=rmax, bw=bw, n=n)
    Mirrored <- stats::density(rseq, weight=NeighborWeight/SumNeighborWeight, to=0, bw=bw)
    # Renormalize density because mirrored distances decreased it
    Density$y <- Density$y / (1 - mean(Mirrored$y)*diff(range(Mirrored$x)))
    
  } else {
    # Classical estimation of distances
    if (inherits(X, "Dtable")) {
      # Dtable case: 
      Dist <- as.vector(Y$Dmatrix[IsReferenceType, IsNeighborType])
      if (Weighted) {
        # Calculate weight products
        Weight <- Y$marks$PointWeight %*% t(Y$marks$PointWeight)
        # Keep useful ones
        Weight <- as.vector(Weight[IsReferenceType, IsNeighborType])
        # Eliminate NAs (for identical reference and neighbor points)
        Weight <- Weight[!is.na(Dist)]
      }
      # Eliminate NAs (for identical reference and neighbor points)
      Dist <- Dist[!is.na(Dist)]
    } else {
    # Prepare a vector for distances between all point pairs.
      if (ReferenceType == NeighborType) {
        # Univariate Kd: n(n-1)/2 pairs
        NbDist <- sum(IsReferenceType)*(sum(IsReferenceType)-1)/2
      } else {
        # Bivariate Kd: n1*n2 pairs
        NbDist <- sum(IsReferenceType)*sum(IsNeighborType)
      } 
      Dist <- vector(mode="double", length=NbDist)
      
      # Prepare a vector for weights if Weighted. Else, set a single value.
      if (Weighted) {
        Weight <- vector(mode="double", length=NbDist)
      } else {
        Weight <- 1
      }
    
      # C++ routine to fill distances and weights
      DistKd(Y$x, Y$y, Y$marks$PointWeight, Weight, Dist, IsReferenceType, IsNeighborType)
    }

    # Min distance obtained from the data rather than 0
    rmin <- ifelse(StartFromMinR, min(Dist), 0)
    if(is.null(r)) {
      # Max distance may be obtained from the data rather than from the window
      if (MaxRange == "DO2005") rmax <- stats::median(Dist)
    }
    
    # Estimate the density. Change the bandwith according to adjust if requested.
    # Only pairs of points up to 2rmax are considered for consistency with approximated computation
    if (Original) {
      bw <- stats::bw.nrd0(Dist[Dist<=rmax*2]) * Adjust
    } else {
      bw <- stats::bw.SJ(Dist[Dist<=rmax*2]) * Adjust
    }
    # Prepare reflection. Distances below rmin + 4bw are mirrored. Code adapted from GoFKernel::density.reflected
    Reflected <- which(Dist <= rmin + 4*bw)
    Dist <- c(Dist, 2*rmin -Dist[Reflected])
    if (Weighted) Weight <- c(Weight, Weight[Reflected])
    # Increase the number of estimation points if necessary. 
    # If rmax << max(Dist), too few estimation points may be below rmax
    # Try to have at least 128 of them. Limit the total number of points to 4096 (i.e. rmax must be >1/32 max(Dist))
    nDensity <- min(max(n, max(Dist)/rmax*128), 4096)
    # Estimate density. Arguments to and from give the range of returned values. Get the mirrored values to calculate their sum.
    if (Weighted) {
      Density <- stats::density(Dist, weights=Weight/sum(Weight), from=rmin, to=rmax, bw=bw, n=nDensity)
      Mirrored <- stats::density(Dist, weights=Weight/sum(Weight), to=rmin, bw=bw)
    } else {
      Density <- stats::density(Dist, from=rmin, to=rmax, bw=bw, n=nDensity)
      Mirrored <- stats::density(Dist, to=rmin, bw=bw)
    }
    # Renormalize density because mirrored distances decreased it
    Density$y <- Density$y / (1- mean(Mirrored$y)*diff(range(Mirrored$x)))
  }
  

  if(is.null(r)) {
    # Return estimated values
    r <- Density$x
    Kd <- Density$y
  } else {
    # Interpolate results at the chosen R
    Kd <- stats::approx(Density$x, Density$y, xout=r)$y    
  }
  KdEstimate <- data.frame(r, Kd)
  colnames(KdEstimate) <- c("r", "Kd")
  
  # Return the values of Kd(r)
  return (fv(KdEstimate, argu="r", ylab=quote(Kd(r)), valu="Kd", fmla= ". ~ r", alim=c(0, max(r)), labl=c("r", paste("hat(%s)(r)", sep="")), desc=c("distance argument r", "Estimated %s"), unitname=X$window$unit, fname="Kd"))
}
