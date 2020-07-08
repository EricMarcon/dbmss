\name{Khat}
\alias{Khat}
\title{
  Estimation of the K function
}
\description{
  Estimates the \emph{K} function
}
\usage{
Khat(X, r = NULL, ReferenceType = "", NeighborType = ReferenceType, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a sensible default value is chosen (512 intervals, from 0 to half the diameter of the window) following \strong{spatstat}.
  }
  \item{ReferenceType}{
  One of the point types. Default is all point types.
  }
  \item{NeighborType}{
  One of the point types. By default, the same as reference type.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{K} is a cumulative, topographic measure of a point pattern structure.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\references{
  Ripley, B. D. (1976). The Foundations of Stochastic Geometry. \emph{Annals of Probability} 4(6): 995-998.
  
  Ripley, B. D. (1977). Modelling Spatial Patterns. \emph{Journal of the Royal Statistical Society B} 39(2): 172-212.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\note{
  The computation of \code{Khat} relies on spatstat functions \code{\link{Kest}} and \code{\link{Kcross}}.
}
\seealso{
  \code{\link{Lhat}}, \code{\link{KEnvelope}}, \code{\link{Ktest}}
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate K
r <- 0:30
(Paracou <- Khat(paracou16, r))

# Plot (after normalization by pi.r^2)
plot(Paracou, ./(pi*r^2) ~ r)
}

