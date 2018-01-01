\name{as.Dtable}
\alias{as.Dtable}
\alias{as.Dtable.ppp}
\alias{as.Dtable.data.frame}
\title{
  Converts data to class Dtable
}
\description{
  Creates an object of class \code{"Dtable"} representing a set of points with weights and labels and the distances between them.. This is a generic method.
}
\usage{
as.Dtable(X, \dots)
  \method{as.Dtable}{ppp}(X, \dots)
  \method{as.Dtable}{data.frame}(X, \dots)
}
\arguments{
  \item{X}{
  Data to be converted into a \code{"Dtable"}.
  }
  \item{\dots}{
  Extra arguments.
  }
}
\details{
  This is a generic method, implemented for \code{\link{ppp}} and \code{\link{data.frame}}. 
  
  Data is first converted to a (\code{\link{wmppp.object}}). Then, the distance matrix between points is calculated and the marks are kept.
}
\value{
  An object of class \code{"Dtable"}.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{as.wmppp}}
}
