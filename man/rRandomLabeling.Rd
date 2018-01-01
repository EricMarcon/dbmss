\name{rRandomLabeling}
\alias{rRandomLabeling}
\title{
  Simulations of a point pattern according to the null hypothesis of random labeling
}
\description{
  Simulates of a point pattern according to the null hypothesis of random labeling.
}
\usage{
rRandomLabeling(X, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  Marks are redistributed randomly across the original point pattern.
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
  \code{\link{rRandomLabelingM}}, \code{\link{rPopulationIndependenceK}}
}
\examples{
# Simulate a point pattern with five types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C", "D", "E"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
X$marks <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

par(mfrow=c(2,2))
plot(X, main="Original pattern, Point Type", which.marks=2)
plot(X, main="Original pattern, Point Weight", which.marks=1)

# Randomize it
Y <- rRandomLabeling(X)
Z <- Y
# Types have been redistributed randomly across locations
plot(Y, main="Randomized pattern, Point Type", which.marks=2)
# weights too
Y <- Z
plot(Y, main="Randomized pattern, Point Weight", which.marks=1)
}
