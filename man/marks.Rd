\name{marks}
\alias{marks.Dtable}
\alias{marks<-.Dtable}
\title{
  marks method for Dtable objects
}
\description{
  S3 methods for the \code{\link{marks}} generic.
}
\usage{
  \method{marks}{Dtable}(x, \dots)
  \method{marks}{Dtable}(x, \dots) <- value
}
\arguments{
  \item{x}{
  A \code{\link{Dtable}} object.
  }
  \item{\dots}{
  Extra arguments, currently unused.
  }
  \item{value}{
  The value to set.
  }
}
\details{
  These functions extract or modify the marks of a \code{\link{Dtable}}.
}
\value{
  A dataframe with columns "PointType" and "PointWeight".
}
\author{
  Eric Marcon <Eric.Marcon@agroparistech.fr>
}
\examples{
# A Dtable containing two points
Dmatrix <- matrix(c(0,1,1,0), nrow=2)
PointType <- c("Type1", "Type2")
PointWeight <- c(2,3)
X <- Dtable(Dmatrix, PointType, PointWeight)
# Extract the marks
marks(X)
}
