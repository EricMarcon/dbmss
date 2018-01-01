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
DistKd(Rx, Ry, RPointWeight, RWeight, RDist, RIsReferenceType, RIsNeighborType)
CountNbdKd(Rr, Rx, Ry, RWeight, RNbd, RIsReferenceType, RIsNeighborType)
}
\arguments{
  \item{Rr}{
  A vector of distances.
  }
  \item{r}{
  A vector of distances.
  }
  \item{Rx}{
  A vector of \emph{x} coordinates.
  }
  \item{x}{
  A vector of \emph{x} coordinates.
  }
  \item{Ry}{
  A vector of \emph{y} coordinates.
  }
  \item{y}{
  A vector of \emph{y} coordinates.
  }
  \item{Dmatrix}{
  A matrix of distances.
  }
  \item{RPointWeight}{
  A vector of point weights.
  }
  \item{RWeight}{
  A vector of point weights.
  }
  \item{Weight}{
  A vector of point weights.
  }
  \item{ReferencePoints}{
  A vector of pont indices.
  }
  \item{RNbd}{
  A matrix to store the weight of neighbors.
  }
  \item{RDist}{
  A vector of point-pair distances.
  }
  \item{RIsReferenceType}{
  A boolean vector to recognize reference points.
  }
  \item{IsReferenceType}{
  A boolean vector to recognize reference points.
  }
  \item{RIsNeighborType}{
  A boolean vector to recognize neighbor points.
  }
  \item{IsNeighborType}{
  A boolean vector to recognize neighbor points.
  }
}
\details{
  These routines are called internally.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\keyword{internal}