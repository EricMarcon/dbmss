\name{wmppp.object}
\alias{wmppp.object}
\title{
  Class of Weighted, Marked, Planar Point Patterns
}
\description{
  A class \code{"wmppp"} to represent a two-dimensional point pattern of class \code{\link{ppp}} whose marks are a dataframe with two columns:
  \itemize{
   \item \code{PointType}: labels, as factors
   \item \code{PointWeight}: weights.
  }
}
\details{
  This class represents a two-dimensional point pattern dataset. \code{wmppp} objects are also of class \code{\link{ppp}}.
  
  Objects of class \code{wmppp} may be created by the function \code{\link{wmppp}} and converted from other types of data by the function \code{\link{as.wmppp}}.
}
\seealso{
  \code{\link{ppp.object}},
  \code{\link{wmppp}},
  \code{\link{as.wmppp}}
}

\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\examples{
# Draw the coordinates of 10 points
X <- runif(10)
Y <- runif(10)
# Draw the point types and weights
PointType   <- sample(c("A", "B"), 10, replace=TRUE)
PointWeight <- runif(10)
# Build the point pattern
X <- wmppp(data.frame(X, Y, PointType, PointWeight), owin())

# Plot the point pattern. which.marks=1 for point weights, 2 for point types
par(mfrow=c(1,2))
plot(X, which.marks=1, main="Point weights")
plot(X, which.marks=2, main="Point types")
}
