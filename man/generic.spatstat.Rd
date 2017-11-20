\name{spatstat generic functions}
\alias{sharpen.wmppp}
\alias{split.wmppp}
\alias{superimpose.wmppp}
\alias{unique.wmppp}
\alias{[.wmppp}
\title{
  Methods for weighted, marked planar point patterns (of class wmppp) from spatstat
}
\description{
  \code{\link{spatstat}} methods for a \code{\link{ppp.object}} applied to a \code{\link{wmppp.object}}.
}
\usage{
\method{sharpen}{wmppp}(\dots)
\method{split}{wmppp}(\dots)
\method{superimpose}{wmppp}(\dots)
\method{unique}{wmppp}(\dots)
\method{[}{wmppp}(\dots)
}
\arguments{
  \item{\dots}{
    Arguments passed to the \code{\link{ppp.object}} method.
  }
}
\details{
  \code{\link{spatstat}} methods for \code{ppp} objects returning a \code{ppp} object can be applied to a \code{wmppp} and return a \code{wpppp} with these methods which just call the \code{\link{ppp.object}} method and change the class of the result for convenience.
  
  Some \code{\link{spatstat}} functions such as \code{\link{rthin}} are not generic so they always return a \code{\link{ppp.object}} when applied to a \code{\link{wmppp.object}}. Their result may be converted by \code{\link{as.wmppp}}.
}
\value{
  An object of class \code{"wmppp"}.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{sharpen.ppp}}, \code{\link{split.ppp}}, \code{\link{superimpose.ppp}}, \code{\link{unique.ppp}}
}

