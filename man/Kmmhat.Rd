\name{Kmmhat}
\alias{Kmmhat}
\title{
  Estimation of the Kmm function
}
\description{
  Estimates of the \emph{Kmm} function
}
\usage{
Kmmhat(X, r = NULL, ReferenceType = "", CheckArguments = TRUE)
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
  The \emph{Kmm} function is used to test the independence of marks.
}
\value{
  An object of class \code{fv}, see \code{\link[spatstat.explore]{fv.object}}, which can be plotted directly using \code{\link[spatstat.explore]{plot.fv}}.
}
\references{
  Penttinen, A., Stoyan, D. and Henttonen, H. M. (1992). Marked Point Processes in Forest Statistics. \emph{Forest Science} 38(4): 806-824.
  
  Penttinen, A. (2006). Statistics for Marked Point Patterns. in \emph{The Yearbook of the Finnish Statistical Society}. The Finnish Statistical Society, Helsinki: 70-91.
}
\note{
  The function is computed using \code{\link[spatstat.explore]{markcorrint}} in spatstat.
}
\seealso{
  \code{\link{Lmmhat}}, \code{\link{LmmEnvelope}}, \code{\link[spatstat.explore]{markcorrint}}
}
\examples{
data(paracou16)
# Keep only 50\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
autoplot(X, 
  labelSize = expression("Basal area (" ~cm^2~ ")"), 
  labelColor = "Species")

# Calculate Kmm
r <- seq(0, 30, 2)
(Paracou <- Kmmhat(X, r))

# Plot
autoplot(Paracou, ./(pi*r^2) ~ r)
}
