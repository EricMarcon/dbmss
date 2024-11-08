\name{marks}
\alias{marks.Dtable}
\alias{marks<-.Dtable}
\alias{marks<-.wmppp}
\title{
  marks method for Dtable and wmppp objects
}
\description{
  S3 methods for the \code{\link{marks}} generic.
}
\usage{
  \method{marks}{Dtable}(x, \dots)
  \method{marks}{Dtable}(x, \dots) <- value
  \method{marks}{wmppp}(x, \dots, dfok = TRUE, drop = TRUE) <- value
}
\arguments{
  \item{x}{
  A \code{\link{Dtable}} or \code{\link{wmppp.object}} object.
  }
  \item{\dots}{
  Extra arguments, currently unused.
  }
  \item{value}{
  The value to set.
  }
  \item{dfok}{
  Ignored.
  }
  \item{drop}{
  Ignored.
  }
}
\details{
  These functions extract or modify the marks of a \code{\link{Dtable}}.
  
  `marks<-.wmppp()` just calls `marks<-.ppp()` and keeps the class of the wmppp object.
  The conformity of the marks with the definition of the class "wmppp", 
  i.e. a dataframe with columns "PointType" and "PointWeight" of the same length
  as the number of points, is not checked.
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
