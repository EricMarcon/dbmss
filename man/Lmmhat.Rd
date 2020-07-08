\name{Lmmhat}
\alias{Lmmhat}
\title{
  Estimation of the Lmm function
}
\description{
  Estimates the \emph{Lmm} function
}
\usage{
Lmmhat(X, r = NULL, ReferenceType = "", CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a sensible default value is chosen (512 intervals, from 0 to half the diameter of the window) following \strong{spatstat}.
  }
  \item{ReferenceType}{
  One of the point types. Others are ignored. Default is all point types.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{Lmm} is the normalized version of \emph{Kmm}: \eqn{Lmm(r)=\sqrt{\frac{Kmm}{\pi}}-r}.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\references{
  Penttinen, A., Stoyan, D. and Henttonen, H. M. (1992). Marked Point Processes in Forest Statistics. \emph{Forest Science} 38(4): 806-824.
  
  Espa, G., Giuliani, D. and Arbia, G. (2010). Weighting Ripley's K-function to account for the firm dimension in the analysis of spatial concentration. \emph{Discussion Papers}, 12/2010. Universita di Trento, Trento: 26.  
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\seealso{
  \code{\link{Kmmhat}}, \code{\link{LmmEnvelope}}
}
\examples{
data(paracou16)
# Keep only 50\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
plot(X)

# Calculate Lmm
r <- seq(0, 30, 2)
(Paracou <- Lmmhat(X, r))

# Plot
plot(Paracou)
}
