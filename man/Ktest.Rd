\name{Ktest}
\alias{Ktest}
\title{
  Test of a point pattern against Complete Spatial Randomness
}
\description{
  Tests the point pattern against CSR using values of the \emph{K} function
}
\usage{
Ktest(X, r)
}
\arguments{
  \item{X}{
  A point pattern (\code{\link{ppp.object}}). Marks are ignored. The window must be a rectangle sensu spatstat (tested by \code{\link{is.rectangle}}).
  }
  \item{r}{
  A vector of distances.
  }
}
\details{
  The test returns the risk to reject CSR erroneously, i.e. the p-value of the test, based on the distribution of the \emph{K} function.
  
  If \code{r} includes 0, it will be silently removed because no neighbor point can be found at distance 0.
  The longer \code{r}, the more accurate the test is in theory but at the cost of computation time first, and of computation accuracy then because a matrix of size the length of \code{r} must be inverted.
  10 values in \code{r} seems to be a reasonable choice.
}
\value{
  A p-value.
}
\references{
  Lang, G. and Marcon, E. (2013). Testing randomness of spatial point patterns with the Ripley statistic. \emph{ESAIM: Probability and Statistics.} 17: 767-788.
  
  Marcon, E., S. Traissac, and Lang, G. (2013). A Statistical Test for Ripley's Function Rejection of Poisson Null Hypothesis. \emph{ISRN Ecology} 2013(Article ID 753475): 9.
}
\author{
  Gabriel Lang <Gabriel.Lang@agroparistech.fr>, Eric Marcon<Eric.Marcon@agroparistech.fr>
}
\seealso{
  \code{\link{Khat}}, \code{\link{GoFtest}}
}
\examples{
# Simulate a Matern (Neyman Scott) point pattern
nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre=c(x0, y0)))
}
X <- rNeymanScott(20, 0.1, nclust, radius=0.2, n=5)
autoplot(as.wmppp(X))

# Test it
Ktest(X, r=seq(0.1, .5, .1))
}
