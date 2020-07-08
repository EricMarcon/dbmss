\name{KinhomEnvelope}
\alias{KinhomEnvelope}
\title{
  Estimation of the confidence envelope of the Kinhom function under its null hypothesis
}
\description{
  Simulates point patterns according to the null hypothesis and returns the envelope of \emph{Kinhom} according to the confidence level.
}
\usage{
KinhomEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05, 
               ReferenceType = "", lambda = NULL, SimulationType = "RandomPosition", 
               Global = FALSE)
}
\arguments{
  \item{X}{
  A point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a sensible default value is chosen (512 intervals, from 0 to half the diameter of the window) following \strong{spatstat}.
  }
  \item{NumberOfSimulations}{
  The number of simulations to run.
  }
  \item{Alpha}{
  The risk level.
  }
  \item{ReferenceType}{
  One of the point types. Default is all point types.
  }
  \item{lambda}{
  An estimation of the point pattern density, obtained by the \code{\link{density.ppp}} function.
  }
  \item{SimulationType}{
  A string describing the null hypothesis to simulate. The null hypothesis, may be 
  "\emph{RandomPosition}": points are drawn in an inhomogenous Poisson process (intensity is either \code{lambda} or estimated from \code{X}); 
  "\emph{RandomLocation}": points are redistributed across actual locations;
  "\emph{RandomLabeling}": randomizes point types, keeping locations unchanged;
  "\emph{PopulationIndependence}": keeps reference points unchanged, redistributes others across actual locations.
  }
  \item{Global}{
  Logical; if \code{TRUE}, a global envelope sensu Duranton and Overman (2005) is calculated. 
  }
}
\details{
  The random location null hypothesis is that of Duranton and Overman (2005). It is appropriate to test the univariate \emph{Kinhom} function of a single point type, redistributing it over all point locations.
  
  The random labeling hypothesis is appropriate for the bivariate \emph{Kinhom} function.
  
  The population independence hypothesis is that of Marcon and Puech (2010).
  
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
  
  Marcon, E. and Puech, F. (2010). Measures of the Geographic Concentration of Industries: Improving Distance-Based Methods. \emph{Journal of Economic Geography} 10(5): 745-762.
  
  Marcon, E. and F. Puech (2017). A typology of distance-based measures of spatial concentration. \emph{Regional Science and Urban Economics}. 62:56-67.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{Kinhomhat}}
}

\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))
plot(X)

# Density of all trees
lambda <- density.ppp(X, bw.diggle(X))
plot(lambda)
V.americana <- X[X$marks$PointType=="V. Americana"]
plot(V.americana, add=TRUE)

# Calculate Kinhom according to the density of all trees
# and confidence envelope (should be 1000 simulations, reduced to 4 to save time)
r <- 0:30
NumberOfSimulations <- 4
Alpha <- .10
plot(KinhomEnvelope(X, r,NumberOfSimulations, Alpha, , 
    SimulationType="RandomPosition", lambda=lambda), ./(pi*r^2) ~ r)
}
