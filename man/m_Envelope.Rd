\name{mEnvelope}
\alias{mEnvelope}
\title{
  Estimation of the confidence envelope of the m function under its null hypothesis
}
\description{
    Simulates point patterns according to the null hypothesis and returns the envelope of \emph{m} according to the confidence level.
}
\usage{
mEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05,
          ReferenceType, NeighborType = ReferenceType, CaseControl = FALSE,
          Original = TRUE, Approximate = ifelse(X$n < 10000, 0, 1), Adjust = 1, 
          MaxRange = "ThirdW", SimulationType = "RandomLocation", Global = FALSE)
}
\arguments{
  \item{X}{
  A point pattern (\code{\link{wmppp.object}}).
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a default value is set: 512 equally spaced values are used up to the median distance between points (following Duranton and Overman, 2005).
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
  \item{Original}{
  Logical; if \code{TRUE} (by default), the original bandwidth selection by Duranton and Overman (2005) following Silverman (1986: eq 3.31) is used. If \code{FALSE}, it is calculated following Sheather and Jones (1991), \emph{i.e.} the state of the art. See \code{\link{bw.SJ}} for more details.
  }
  \item{Approximate}{
  if not 0 (1 is a good choice), exact distances between pairs of points are rounded to 1024 times \code{Approximate} single values equally spaced between 0 and the largest distance. This technique (Scholl and Brenner, 2015) allows saving a lot of memory when addressing large point sets (the default value is 1 over 10000 points). Increasing \code{Approximate} allows better precision at the cost of proportional memory use.
  }
  \item{Adjust}{
  Force the automatically selected bandwidth (following \code{Original}) to be multiplied by \code{Adjust}. Setting it to values lower than one (1/2 for example) will sharpen the estimation. 
  }
  \item{MaxRange}{
  The maximum value of \code{r} to consider, ignored if \code{r} is not \code{NULL}. Default is "ThirdW", one third of the diameter of the window. Other choices are "HalfW", and "QuarterW" and "D02005".
  "HalfW", and "QuarterW" are for half or the quarter of the diameter of the window.
  "D02005" is for the median distance observed between points, following Duranton and Overman (2005). "ThirdW" should be close to "DO2005" but has the advantage to be independent of the point types chosen as \code{ReferenceType} and \code{NeighborType}, to simplify comparisons between different types. "D02005" is approximated by "ThirdW" if \code{Approximate} is not 0.
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

  Lang G., Marcon E. and Puech F. (2014) Distance-Based Measures of Spatial Concentration: Introducing a Relative Density Function. \emph{HAL} 01082178, 1-18.

  Loosmore, N. B. and Ford, E. D. (2006). Statistical inference using the G or K point pattern spatial statistics. \emph{Ecology} 87(8): 1925-1931.
  
  Marcon, E. and F. Puech (2017). A typology of distance-based measures of spatial concentration. \emph{Regional Science and Urban Economics}. 62:56-67.
  
  Scholl, T. and Brenner, T. (2015) Optimizing distance-based methods for large data sets, \emph{Journal of Geographical Systems} 17(4): 333-351.

  Silverman, B. W. (1986). \emph{Density estimation for statistics and data analysis}. Chapman and Hall, London.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{mhat}}
}

\examples{
data(paracou16)
# Keep only 50\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
plot(X)

# Calculate confidence envelope (should be 1000 simulations, reduced to 4 to save time)
NumberOfSimulations <- 4
Alpha <- .10
plot(mEnvelope(X, , NumberOfSimulations, Alpha, 
    "V. Americana", "Q. Rosea", Original = FALSE, SimulationType = "RandomLabeling"))
}
