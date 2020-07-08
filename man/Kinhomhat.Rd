\name{Kinhomhat}
\alias{Kinhomhat}
\title{
  Estimation of the inhomogenous K function
}
\description{
  Estimates the \emph{Kinhom} function
}
\usage{
Kinhomhat(X, r = NULL, ReferenceType = "", lambda = NULL, CheckArguments = TRUE)
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
  \item{lambda}{
  An estimation of the point pattern density, obtained by the \code{\link{density.ppp}} function.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{Kinhom} is a cumulative, topographic measure of an inhomogenous point pattern structure.
  
  By default, density estimation is performed at points by \code{\link{density.ppp}} using the optimal bandwith (\code{\link{bw.diggle}}). It can be calculated separately (see example), including at pixels if the point pattern is too large for the default estimation to succeed, and provided as the argument \code{lambda}:
  Arbia et al. (2012) for example use another point pattern as a reference to estimate density.
  
  Bivariate \emph{Kinhom} is not currently supported.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\references{
  Baddeley, A. J., J. Moller, et al. (2000). Non- and semi-parametric estimation of interaction in inhomogeneous point patterns. \emph{Statistica Neerlandica} 54(3): 329-350.
  
  Arbia, G., G. Espa, et al. (2012). Clusters of firms in an inhomogeneous space: The high-tech industries in Milan. \emph{Economic Modelling} 29(1): 3-11.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\note{
  The computation of \code{Kinhomhat} relies on spatstat functions \code{\link{Kinhom}}, \code{\link{density.ppp}} and \code{\link{bw.diggle}}.
}
\seealso{
  \code{\link{KinhomEnvelope}}, \code{\link{Kinhom}}
}
\examples{
data(paracou16)

# Density of all trees
lambda <- density.ppp(paracou16, bw.diggle(paracou16))
plot(lambda)
# Reduce the point pattern to one type of trees
V.americana <- paracou16[paracou16$marks$PointType=="V. Americana"]
plot(V.americana, add=TRUE)

# Calculate Kinhom according to the density of all trees
r <- 0:30
plot(Kinhomhat(paracou16, r, "V. Americana", lambda), ./(pi*r^2) ~ r)
}
