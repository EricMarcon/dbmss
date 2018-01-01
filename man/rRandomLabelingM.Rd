\name{rRandomLabelingM}
\alias{rRandomLabelingM}
\title{
  Simulations of a point pattern according to the null hypothesis of random labelling defined for M
}
\description{
  Simulates of a point pattern according to the null hypothesis of random labelling defined for \emph{M}
}
\usage{
rRandomLabelingM(X, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}) or a \code{\link{Dtable}} object.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  Point types are randomized. Locations and weights are kept unchanged. If both types and weights must be randomized together (Duranton and Overman, 2005; Marcon and Puech, 2010), use \code{\link{rRandomLocation}}.
}
\value{
  A new weighted, marked, planar point pattern (an object of class \code{wmppp}, see \code{\link{wmppp.object}}).
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.

  Marcon, E. and Puech, F. (2010). Measures of the Geographic Concentration of Industries: Improving Distance-Based Methods. \emph{Journal of Economic Geography} 10(5): 745-762.
  
  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. \emph{International Journal of Ecology} 2012(Article ID 619281): 11.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\seealso{
  \code{\link{rRandomLabeling}}, \code{\link{rPopulationIndependenceM}}
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
Y <- rRandomLabelingM(X)
Z <- Y
# Labels have been redistributed randomly across locations
plot(Y, main="Randomized pattern, Point Type", which.marks=2)
# But weights are unchanged
Y <- Z
plot(Y, main="Randomized pattern, Point Weight", which.marks=1)
}
