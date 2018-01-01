\name{FillEnvelope}
\alias{FillEnvelope}
\title{
  Transform simulation values to an fv
}
\description{
  This function is used internally to calculate envelope values and store them into an \code{\link{fv.object}}.
}
\usage{
FillEnvelope(Envelope, Alpha, Global)
}
\arguments{
  \item{Envelope}{
  An envelope object (\code{\link{envelope}}) containing all the simulated function values.
  }
  \item{Alpha}{
  The risk level.
  }
  \item{Global}{
  Logical; if \code{TRUE}, a global envelope sensu Duranton and Overman (2005) is calculated. 
  }
}
\value{
  Returns the envelope object (\code{\link{envelope}}) with \code{hi} and \code{lo} values calculated from the simlations.
}

\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}


\keyword{internal}
