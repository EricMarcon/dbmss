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
  Points marks are kept unchanged and their position is drawn in a binomial process by \code{\link{runifpoint}}.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\note{
  Simulations in a binomial process keeps the same number of points, so that marks can be redistributed. If a real CSR simulation is needed and marks are useless, use \code{\link{rpoispp}}.
  
  Actual data coordinates are often rounded. Use the \code{Precision} argument to simulate point patterns with the same rounding procedure. For example, if point coordinates are in meters and rounded to the nearest half meter, use \code{Precision = 0.5} so that the same approximation is applied to the simulated point patterns.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\seealso{
  \code{\link{rRandomLocation}}
}
\examples{
# Simulate a point pattern with two types
X <- rpoispp(5) 
PointType   <- sample(c("A", "B"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
X$marks <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

par(mfrow=c(1,2))
plot(X, main="Original pattern, Point Type")

# Randomize it
Y <- rRandomPositionK(X)
# Invert the order of columns in mark to plot the point type, not the point weight
Y$marks <- data.frame(Y$marks$PointType, Y$marks$PointWeight)
# Points are randomly distributed
plot(Y, main="Randomized pattern, Point Type")
}