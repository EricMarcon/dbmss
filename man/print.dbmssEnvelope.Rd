\name{print.dbmssEnvelope}
\alias{print.dbmssEnvelope}
\title{
  Print a confidence envelope
}
\description{
  Prints useful information of a confidence envelope of class "dbmssEnvelope"
}
\usage{
\method{print}{dbmssEnvelope}(x, \dots)
}
\arguments{
  \item{x}{
  An object of class "dbmssEnvelope".
  }
  \item{\dots}{
  Ignored.
  }
}
\details{
  "dbmssEnvelope" objects are similar to \code{\link{envelope}} objects. The way they are printed is different to take into account the possibility of building global envelope following Duranton and Overman (2005):  the global envelope is calculated by iteration: the simulations reaching one of the upper or lower values at any distance are eliminated at each step. The process is repeated until \emph{Alpha / Number of simulations} simulations are dropped. The remaining upper and lower bounds at all distances constitute the global envelope. Interpolation is used if the exact ratio cannot be reached.
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate intertype K envelope
Envelope <- KEnvelope(paracou16, NumberOfSimulations = 20, Global = TRUE,
  ReferenceType = "V. Americana", NeighborType = "Q. Rosea")
plot(Envelope)
# print
print(Envelope)
}
