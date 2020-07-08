\name{DEnvelope}
\alias{DEnvelope}
\title{
  Estimation of the confidence envelope of the D function under its null hypothesis
}
\description{
  Simulates point patterns according to the null hypothesis and returns the envelope of \emph{D} according to the confidence level.
}
\usage{
DEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05, 
          Cases, Controls, Intertype = FALSE, Global = FALSE)
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
  \item{Cases}{
  One of the point types
  }
  \item{Controls}{
  One of the point types.
  }
  \item{Intertype}{
  Logical; if \code{TRUE}, \emph{D} is computed as \emph{Di} in Marcon and Puech (2012). 
  }
  \item{Global}{
  Logical; if \code{TRUE}, a global envelope sensu Duranton and Overman (2005) is calculated. 
  }
}
\details{
  The only null hypothesis is random labeling: marks are distributed randomly across points.
  
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
  \code{\link{Dhat}}
}

\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))
plot(X)

# Calculate confidence envelope (should be 1000 simulations, reduced to 20 to save time)
r <- 0:30
NumberOfSimulations <- 20
Alpha <- .05
# Plot the envelope (after normalization by pi.r^2)
plot(DEnvelope(X, r, NumberOfSimulations, Alpha,
    "V. Americana", "Q. Rosea", Intertype = TRUE), ./(pi*r^2) ~ r)
}
