\name{rRandomPositionK}
\alias{rRandomPositionK}
\title{
  Simulations of a point pattern according to the null hypothesis of random position defined for K
}
\description{
  Simulations of a point pattern according to the null hypothesis of random position defined for \emph{K}.
}
\usage{
rRandomPositionK(X, Precision = 0, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{Precision}{
  Accuracy of point coordinates, measured as a part of distance unit. See notes. Default is 0 for no approximation.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  Points marks are kept unchanged and their position is drawn in a binomial process by \code{\link[spatstat.random]{runifpoint}}.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\note{
  Simulations in a binomial process keeps the same number of points, so that marks can be redistributed. If a real CSR simulation is needed and marks are useless, use \code{\link[spatstat.random]{rpoispp}}.
  
  Actual data coordinates are often rounded. Use the \code{Precision} argument to simulate point patterns with the same rounding procedure. For example, if point coordinates are in meters and rounded to the nearest half meter, use \code{Precision = 0.5} so that the same approximation is applied to the simulated point patterns.
}
\seealso{
  \code{\link{rRandomLocation}}
}
\examples{
# Simulate a point pattern with two types
X <- rpoispp(5) 
PointType   <- sample(c("A", "B"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
marks(X) <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

autoplot(X, main="Original pattern")

# Randomize it
Y <- rRandomPositionK(X)
# Points are randomly distributed
autoplot(Y, main="Randomized pattern")
}
