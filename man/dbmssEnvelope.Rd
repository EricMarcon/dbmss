\name{dbmssEnvelope.object}
\alias{dbmssEnvelope.object}
\title{
  Class of envelope of function values (fv)
}
\description{
  A class \code{"dbmssEnvelope"}, \emph{i.e.} a particular type of see \code{\link{envelope}} to represent several estimates of the same function and its confidence envelope.
}
\details{
  "dbmssEnvelope" objects are similar to envelope objects. The differences are that the risk level is chosen (instead of the simulation rank to use as the envelope), so the rank is calculated (interpolation is used if necessary), and a global envelope can be calculated following Duranton and Overman (2005).
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{summary.dbmssEnvelope}},
  \code{\link{KdEnvelope}},
  \code{\link{MEnvelope}}
}

 
