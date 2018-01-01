\name{GlobalEnvelope}
\alias{GlobalEnvelope}
\title{
  Estimation of the global confidence interval of simulations
}
\description{
  Calculates the global confidence interval envelope sensu Duranton and Overman (2005) according to simulations of the null hypothesis of a function.
}
\usage{
GlobalEnvelope(Simulations, Alpha)
}
\arguments{
  \item{Simulations}{
  An object of class \code{fv}, see \code{\link{fv.object}}, containing the simulated values.
  }
  \item{Alpha}{
  The risk level.
  }
}
\details{
  The global envelope is calculated by iteration: the simulations reaching one of the upper or lower values at any distance are eliminated at each step. The process is repeated until \emph{Alpha / Number of simulations} simulations are dropped. The remaining upper and lower bounds at all distances constitute the global envelope. Interpolation is used if the exact ratio cannot be reached.
}
\value{
A matrix with two lines:
  \item{Glo}{the lower bound of the envelope}
  \item{Ghi}{the upper bound of the envelope}
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.  
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}

\keyword{internal}