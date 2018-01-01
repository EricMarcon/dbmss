\name{MEnvelope}
\alias{MEnvelope}
\title{
  Estimation of the confidence envelope of the M function under its null hypothesis
}
\description{
    Simulates point patterns according to the null hypothesis and returns the envelope of \emph{M} according to the confidence level.
}
\usage{
MEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05,
          ReferenceType, NeighborType = ReferenceType, 
          CaseControl = FALSE, SimulationType = "RandomLocation", Global = FALSE)
}
\arguments{
  \item{X}{
  A point pattern (\code{\link{wmppp.object}}) or a \code{\link{Dtable}} object.
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a default value is set: 32 unequally spaced values are used up to half the maximum distance between points \eqn{d_m}. The first value is 0, first steps are small (\eqn{d_m/200}) then incresase progressively up to \eqn{d_m/20}.
  }
  \item{NumberOfSimulations}{
  The number of simulations to run, 100 by default.
  }
  \item{Alpha}{
  The risk level, 5\% by default.
  }
  \item{ReferenceType}{
  One of the point types.
  }
  \item{NeighborType}{
  One of the point types, equal to the reference type by default to caculate univariate M.
  }
  \item{CaseControl}{
  Logical; if \code{TRUE}, the case-control version of \emph{M} is computed. \emph{ReferenceType} points are cases, \emph{NeighborType} points are controls.
  }
  \item{SimulationType}{
  A string describing the null hypothesis to simulate. The null hypothesis may be
  "\emph{RandomLocation}": points are redistributed on the actual locations (default);
  "\emph{RandomLabeling}": randomizes point types, keeping locations and weights unchanged;
  "\emph{PopulationIndependence}": keeps reference points unchanged, randomizes other point locations.
  }
  \item{Global}{
  Logical; if \code{TRUE}, a global envelope sensu Duranton and Overman (2005) is calculated. 
  }
}
\details{
  This envelope is local by default, that is to say it is computed separately at each distance. See Loosmore and Ford (2006) for a discussion.
  
  The global envelope is calculated by iteration: the simulations reaching one of the upper or lower values at any distance are eliminated at each step. The process is repeated until \emph{Alpha / Number of simulations} simulations are dropped. The remaining upper and lower bounds at all distances constitute the global envelope. Interpolation is used if the exact ratio cannot be reached.
}
\value{
  An envelope object (\code{\link{envelope}}). There are methods for print and plot for this class.
  
  The \code{fv} contains the observed value of the function, its average simulated value and the confidence envelope.
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.
  
  Kenkel, N. C. (1988). Pattern of Self-Thinning in Jack Pine: Testing the Random Mortality Hypothesis. \emph{Ecology} 69(4): 1017-1024.
  
  Loosmore, N. B. and Ford, E. D. (2006). Statistical inference using the G or K point pattern spatial statistics. \emph{Ecology} 87(8): 1925-1931.
  
  Marcon, E. and F. Puech (2017). A typology of distance-based measures of spatial concentration. \emph{Regional Science and Urban Economics}. 62:56-67.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{Mhat}}
}

\examples{
data(paracou16)
# Keep only 50\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
plot(X)

# Calculate confidence envelope (should be 1000 simulations, reduced to 4 to save time)
NumberOfSimulations <- 4
Alpha <- .10
plot(MEnvelope(X, , NumberOfSimulations, Alpha, 
    "V. Americana", "Q. Rosea", FALSE, "RandomLabeling"))
}
