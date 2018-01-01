\name{Dtable}
\alias{Dtable}
\title{
  Create a Distance table object.
}
\description{
  Creates an object of class \code{"Dtable"} representing a set of points with weights and labels and the distances between them.
}
\usage{
Dtable(Dmatrix, PointType = NULL, PointWeight = NULL)
}
\arguments{
  \item{Dmatrix}{
  A square matrix containing distances.
  }
  \item{PointType}{
  A vector describing the point types. Its length must correspond to the number of points. 
  }
  \item{PointWeight}{
  A vector describing the point weights. Its length must correspond to the number of points.
  }
}
\details{
  The distance matrix is not necessarily symmetric, so distances are understood in the common sense, not in the mathematical sense. Asymmetric distances are appropriate when paths between points are one-way only.
  
  The points of origin are in lines, the targets in columns. The diagonal of the matrix must contain zeros (the distance between a point and itself is 0), and all other distances must be positive (they can be 0).
}
\value{
  An object of class \code{"Dtable"}. It is a list:
  \item{Dmatrix}{The distance matrix.}
  \item{n}{The number of points.}
  \item{marks}{A list of two items: \code{PointType}, a vector of factors containing the point types and \code{PointWeight}, the numeric vector of weights.}
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{as.Dtable}}
}
\examples{
# A Dtable containing two points
Dmatrix <- matrix(c(0,1,1,0), nrow=2)
PointType <- c("Type1", "Type2")
PointWeight <- c(2,3)
Dtable(Dmatrix, PointType, PointWeight)
}
