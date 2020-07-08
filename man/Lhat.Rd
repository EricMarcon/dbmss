\name{Lhat}
\alias{Lhat}
\title{
  Estimation of the L function
}
\description{
  Estimates the \emph{L} function
}
\usage{
Lhat(X, r = NULL, ReferenceType = "", NeighborType = "", CheckArguments = TRUE)
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
  One of the point types. Default is all point types.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{L} is the normalized version of \emph{K}: \eqn{L(r)=\sqrt{\frac{K}{\pi}}-r}.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\references{
  Besag, J. E. (1977). Comments on Ripley's paper. \emph{Journal of the Royal Statistical Society B} 39(2): 193-195.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\note{
  \emph{L} was originally defined as \eqn{L(r)=\sqrt{\frac{K}{\pi}}}. It has been used as \eqn{L(r)=\sqrt{\frac{K}{\pi}}-r} in a part of the literature because this normalization is easier to plot.
}

\seealso{
  \code{\link{Khat}}, \code{\link{LEnvelope}}
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate L
r <- 0:30
(Paracou <- Lhat(paracou16, r))

# Plot
plot(Paracou)
}