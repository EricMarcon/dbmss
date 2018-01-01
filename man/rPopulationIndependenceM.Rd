\name{rPopulationIndependenceM}
\alias{rPopulationIndependenceM}
\title{
  Simulations of a point pattern according to the null hypothesis of population independence defined for M
}
\description{
  Simulates of a point pattern according to the null hypothesis of population independence defined for \emph{M}
}
\usage{
rPopulationIndependenceM(X, ReferenceType, CheckArguments = TRUE)
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
  Reference points are kept unchanged, other points are redistributed randomly across locations.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\references{
  Marcon, E. and Puech, F. (2010). Measures of the Geographic Concentration of Industries: Improving Distance-Based Methods. \emph{Journal of Economic Geography} 10(5): 745-762.
  
  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. \emph{International Journal of Ecology} 2012(Article ID 619281): 11.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\seealso{
  \code{\link{rPopulationIndependenceK}}, \code{\link{rRandomLabelingM}}
}
\examples{
# Simulate a point pattern with five types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C", "D", "E"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
X$marks <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

par(mfrow=c(1,2))
plot(X, main="Original pattern, Point Type", which.marks=2)

# Randomize it
Y <- rPopulationIndependenceM(X, "A")
# Points of type "A" (circles) are unchanged, 
# all other points have been redistributed randomly across locations
plot(Y, main="Randomized pattern, Point Type", which.marks=2)
}
