\name{as.wmppp}
\alias{as.wmppp}
\alias{as.wmppp.ppp}
\alias{as.wmppp.data.frame}
\title{
  Converts data to class wmppp
}
\description{
  Creates a Weighted, Marked, Planar Point Pattern, \emph{i.e.} an object of class \code{"wmppp"} representing a two-dimensional point pattern with weights and labels. This is a generic method.
}
\usage{
as.wmppp(X, \dots)
  \method{as.wmppp}{ppp}(X, \dots)
  \method{as.wmppp}{data.frame}(X, window = NULL, unitname = NULL, \dots)
}
\arguments{
  \item{X}{
  Data to be converted into a weighted, marked, planar point pattern (\code{\link{wmppp.object}})
  }
  \item{window}{
  An object of calls "owin" (\code{\link{owin.object}}).  
  }
  \item{unitname}{
  Name of unit of length. Either a single character string, or a vector of two character strings giving the singular and plural forms, respectively.  
  }
  \item{\dots}{
  Extra arguments.
  }
}
\details{
  This is a generic method, implemented for \code{\link{ppp}} and \code{\link{data.frame}}:
  \itemize{
    \item{If the dataset \code{X} is an object of class "ppp" (\code{\link{ppp.object}}), the marks are converted to point weights if they are numeric or to point types if they are factors. Default weights are set to 1, default types to "All". If marks are a dataframe with column names equal to \code{PointType} and \code{PointWeight}, they are not modified. Row namles of the dataframe are preserved aqs row names of the marks, to identify points.
    }
    \item{If the dataset \code{X} is a dataframe, see \code{\link{wmppp}}.
    }
  }
}
\value{
  An object of class \code{"wmppp"}.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{wmppp.object}}
}
