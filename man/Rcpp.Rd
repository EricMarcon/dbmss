\name{parallelCountNbd}
\alias{parallelCountNbd}
\alias{parallelCountNbdDt}
\alias{parallelCountNbdCC}
\alias{parallelCountNbdDtCC}
\alias{parallelCountNbdm}
\alias{DistKd}
\alias{CountNbdKd}
\title{
  Auxiliary functions to count point neighbors
}
\description{
  C++ routines used for fast count of neighbors.
}
\usage{
parallelCountNbd(r, x, y, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdCC(r, x, y, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdm(x, y, ReferencePoints)
parallelCountNbdDt(r, Dmatrix, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdDtCC(r, Dmatrix, Weight, IsReferenceType, IsNeighborType)
DistKd(x, y, PointWeight, Weight, Dist, IsReferenceType, IsNeighborType)
CountNbdKd(r, x, y, Weight, Nbd, IsReferenceType, IsNeighborType)
}
\arguments{
  \item{r}{
  A vector of distances.
  }
  \item{x}{
  A vector of \emph{x} coordinates.
  }
  \item{y}{
  A vector of \emph{y} coordinates.
  }
  \item{Dmatrix}{
  A matrix of distances.
  }
  \item{PointWeight}{
  A vector of point weights.
  }
  \item{Weight}{
  A vector of point weights.
  }
  \item{ReferencePoints}{
  A vector of pont indices.
  }
  \item{Nbd}{
  A matrix to store the weight of neighbors.
  }
  \item{Dist}{
  A vector of point-pair distances.
  }
  \item{IsReferenceType}{
  A boolean vector to recognize reference points.
  }
  \item{IsNeighborType}{
  A boolean vector to recognize neighbor points.
  }
}
\details{
  These routines are called internally.
}
\keyword{internal}
