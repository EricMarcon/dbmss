\name{rRandomLocation}
\alias{rRandomLocation}
\title{
  Simulations of a point pattern according to the null hypothesis of random location
}
\description{
  Simulates of a point pattern according to the null hypothesis of random location.
}
\usage{
rRandomLocation(X, ReferenceType = "", CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{ReferenceType}{
  One of the point types.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  Points are redistributed randomly across the locations of the original point pattern. This randomization is equivalent to random labeling, considering the label is both point type and point weight.
  If \code{ReferenceType} is specified, then only reference type points are kept in the orginal point pattern before randomization.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.
  
  Marcon, E. and Puech, F. (2010). Measures of the Geographic Concentration of Industries: Improving Distance-Based Methods. \emph{Journal of Economic Geography} 10(5): 745-762.
}
\seealso{
  \code{\link{rRandomPositionK}}
}
\examples{
# Simulate a point pattern with five types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C", "D", "E"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
X$marks <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

autoplot(X, main="Original pattern")

# Randomize it
Y <- rRandomLocation(X)
# Points have been redistributed randomly across locations
autoplot(Y, main="Randomized pattern")
}
