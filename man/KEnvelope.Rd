\name{KEnvelope}
\alias{KEnvelope}
\title{
  Estimation of the confidence envelope of the K function under its null hypothesis
}
\description{
    Simulates point patterns according to the null hypothesis and returns the envelope of \emph{K} according to the confidence level.
}
\usage{
KEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05, 
          ReferenceType = "", NeighborType = ReferenceType, 
          SimulationType = "RandomPosition", Precision = 0, Global = FALSE)
}
\arguments{
  \item{X}{
  A point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a sensible default value is chosen (512 intervals, from 0 to half the diameter of the window) following \strong{spatstat}.
  }
  \item{NumberOfSimulations}{
  The number of simulations to run, 100 by default.
  }
  \item{Alpha}{
  The risk level, 5\% by default.
  }
  \item{ReferenceType}{
  One of the point types. Default is all point types.
  }
  \item{NeighborType}{
  One of the point types. By default, the same as reference type.
  }
  \item{SimulationType}{
  A string describing the null hypothesis to simulate. The null hypothesis may be
  "\emph{RandomPosition}": points are drawn in a Poisson process (default);
  "\emph{RandomLabeling}": randomizes point types, keeping locations unchanged;
  "\emph{PopulationIndependence}": keeps reference points unchanged, shifts other point locations.
  }
  \item{Precision}{
  Accuracy of point coordinates, measured as a part of distance unit. See \code{\link{rRandomPositionK}}. Default is 0 for no approximation.
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

  Silverman, B. W. (1986). \emph{Density estimation for statistics and data analysis}. Chapman and Hall, London.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{Khat}}, \code{\link{rRandomPositionK}}, \code{\link{rRandomLocation}}, \code{\link{rPopulationIndependenceK}}
}

\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))
plot(X)

# Calculate confidence envelope (should be 1000 simulations, reduced to 20 to save time)
r <- 0:30
NumberOfSimulations <- 20
# Plot the envelope
plot(KEnvelope(X, r, NumberOfSimulations), ./(pi*r^2) ~ r)
}
