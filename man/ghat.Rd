\name{ghat}
\alias{ghat}
\title{
  Estimation of the g function
}
\description{
  Estimates the \emph{g} function
}
\usage{
ghat(X, r = NULL, ReferenceType = "", NeighborType = "", CheckArguments = TRUE)
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
  The computation of \code{ghat} relies on spatstat function \code{\link[spatstat.explore]{sewpcf}}.
  The kernel estimation of the number of neighbors follows Stoyan and Stoyan (1994, pages 284–285).
}
\value{
  An object of class \code{fv}, see \code{\link[spatstat.explore]{fv.object}}, which can be plotted directly using \code{\link[spatstat.explore]{plot.fv}}.
}
\references{
  Stoyan, D. and Stoyan, H. (1994) \emph{Fractals, random shapes and point fields: methods of geometrical statistics}. John Wiley and Sons.
}
\seealso{
  \code{\link{gEnvelope}}
}
\examples{
data(paracou16)
autoplot(paracou16)

# Calculate g
r <- 0:30
(Paracou <- ghat(paracou16, r, "Q. Rosea", "V. Americana"))

# Plot
autoplot(Paracou)
}
