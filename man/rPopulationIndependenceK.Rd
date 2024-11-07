\name{rPopulationIndependenceK}
\alias{rPopulationIndependenceK}
\title{
  Simulations of a point pattern according to the null hypothesis of population independence defined for K
}
\description{
  Simulates of a point pattern according to the null hypothesis of population independence defined for \emph{K}.
}
\usage{
rPopulationIndependenceK(X, ReferenceType, NeighborType, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{ReferenceType}{
  One of the point types.
  }
  \item{NeighborType}{
  One of the point types.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  Reference points are kept unchanged, neighbor type point positions are shifted by \code{\link[spatstat.random]{rshift}}.
  Other points are lost and point weights are not kept (they are set to 1) since the K function ignores them.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\references{
  Goreaud, F. et Pelissier, R. (2003). Avoiding misinterpretation of biotic interactions with the intertype K12 fonction: population independence vs random labelling hypotheses. \emph{Journal of Vegetation Science} 14(5): 681-692.
}
\seealso{
  \code{\link{rPopulationIndependenceM}}, \code{\link{rRandomLabeling}}
}
\examples{
# Simulate a point pattern with three types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
marks(X) <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

# Plot the point pattern, using PointType as marks
autoplot(X, main="Original pattern")

# Randomize it
Y <- rPopulationIndependenceK(X, "A", "B")
# Points of type "A" are unchanged, points of type "B" have been moved altogether
# Other points are lost and point weights are set to 1
autoplot(Y, main="Randomized pattern")
}
