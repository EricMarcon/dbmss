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
  Reference points are kept unchanged, other point positions are shifted by \code{\link{rshift}}.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\references{
  Goreaud, F. et Pelissier, R. (2003). Avoiding misinterpretation of biotic interactions with the intertype K12 fonction: population independence vs random labelling hypotheses. \emph{Journal of Vegetation Science} 14(5): 681-692.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\seealso{
  \code{\link{rPopulationIndependenceM}}, \code{\link{rRandomLabeling}}
}
\examples{
# Simulate a point pattern with two types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
X$marks <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

par(mfrow=c(1,2))
# Plot the point pattern, using PointType as marks
plot(X, main="Original pattern, Point Type", which.marks=2)

# Randomize it
Y <- rPopulationIndependenceK(X, "A", "B")
# Points of type "A" are unchanged, points of type "B" have been moved altogether
plot(Y, main="Randomized pattern, Point Type", which.marks=2)
}
