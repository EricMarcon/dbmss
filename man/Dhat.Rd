\name{Dhat}
\alias{Dhat}
\title{
  Estimation of the D function
}
\description{
  Estimates the \emph{D} function}
\usage{
Dhat(X, r = NULL, Cases, Controls = NULL, Intertype = FALSE, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked, planar point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a sensible default value is chosen (512 intervals, from 0 to half the diameter of the window) following \strong{spatstat}.
  }
  \item{Cases}{
  One of the point types.
  }
  \item{Controls}{
  One of the point types. If \code{NULL}, controls are all types except for cases.
  }
  \item{Intertype}{
  Logical; if \code{TRUE}, \emph{D} is computed as \emph{Di} in Marcon and Puech (2012). 
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  The \emph{Di} function allows comparing the structure of the cases to that of the controls around cases, that is to say the comparison is made around the same points. This has been advocated by Arbia et al. (2008) and formalized by Marcon and Puech (2012).
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\references{
  Arbia, G., Espa, G. and Quah, D. (2008). A class of spatial econometric methods in the empirical analysis of clusters of firms in the space. \emph{Empirical Economics} 34(1): 81-103.
  
  Diggle, P. J. and Chetwynd, A. G. (1991). Second-Order Analysis of Spatial Clustering for Inhomogeneous Populations. \emph{Biometrics} 47(3): 1155-1163.
  
  Marcon, E. and F. Puech (2017). A typology of distance-based measures of spatial concentration. \emph{Regional Science and Urban Economics}. 62:56-67.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\note{
  The computation of \code{Dhat} relies on spatstat functions \code{\link{Kest}} and \code{\link{Kcross}}.
}

\seealso{
  \code{\link{Khat}}, \code{\link{DEnvelope}}, \code{\link{Kest}}, \code{\link{Kcross}}
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate D
r <- 0:30
(Paracou <- Dhat(paracou16, r, "V. Americana", "Q. Rosea", Intertype = TRUE))

# Plot (after normalization by pi.r^2)
plot(Paracou, ./(pi*r^2) ~ r)
}
