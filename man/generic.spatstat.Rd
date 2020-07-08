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
  \strong{spatstat} methods for a \code{\link{ppp.object}} applied to a \code{\link{wmppp.object}}.
}
\usage{
\method{sharpen}{wmppp}(X, \dots)
\method{split}{wmppp}(\dots)
\method{superimpose}{wmppp}(\dots)
\method{unique}{wmppp}(x, \dots)
\method{[}{wmppp}(i, j, drop=FALSE, \dots, clip=FALSE)
}
\arguments{
  \item{X, x}{
  A two-dimensional point pattern. An object of class "wmppp".
  }
  \item{\dots}{
  Arguments passed to the \code{\link{ppp.object}} method.
  }
  \item{i}{
  Subset index. Either a valid subset index in the usual R sense, indicating which points should be retained, or a window (an object of class "owin") delineating a subset of the original observation window, or a pixel image with logical values defining a subset of the original observation window.
  }
  \item{j}{
  Redundant. Included for backward compatibility.
  }
  \item{drop}{
  Logical value indicating whether to remove unused levels of the marks, if the marks are a factor.
  }
  \item{clip}{
  Logical value indicating how to form the window of the resulting point pattern, when \code{i} is a window. If \code{clip=FALSE} (the default), the result has window equal to \code{i}. If \code{clip=TRUE}, the resulting window is the intersection between the window of \code{x} and the window \code{i}.
  }
}
\details{
  \strong{spatstat} methods for \code{ppp} objects returning a \code{ppp} object can be applied to a \code{wmppp} and return a \code{wpppp} with these methods which just call the \code{\link{ppp.object}} method and change the class of the result for convenience.
  
  Some \strong{spatstat} functions such as \code{\link{rthin}} are not generic so they always return a \code{\link{ppp.object}} when applied to a \code{\link{wmppp.object}}. Their result may be converted by \code{\link{as.wmppp}}.
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

