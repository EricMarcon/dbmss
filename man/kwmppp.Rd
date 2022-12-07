\name{kwmppp}
\alias{kwmppp}
\alias{plot.kwmppp}
\title{Kriged, Weighted, Marked, Planar point pattern}
\usage{
  kwmppp(X, fvind, ReferenceType = "", distance = stats::median(fvind$r), 
         AllowJitter = TRUE, Nbx = 128, Nby = 128, CheckArguments = TRUE)
  \method{plot}{kwmppp} (x, \dots, Contour = TRUE, 
  Palette = grDevices::topo.colors(128, alpha=1), 
  SuppressMargins = TRUE, Contournlevels = 10, Contourcol = "dark red")

}
\description{
  Kriged, Weighted, Marked, Planar point pattern are maps of the individiual values of dbmss's.
}
\arguments{
  \item{X}{
    A point pattern (\code{\link{wmppp.object}}).
  }
  \item{fvind}{
    An object of class \code{fv}, see \code{\link{fv.object}}, obtained a distance-based method, such as \code{\link{Mhat}} with individual values (argument \code{Individual = TRUE}). 
  }
  \item{ReferenceType}{
    The point type used to calculate the function values.
    The default value is "", i.e. all point types, but it will generate an error if the actual reference type is different.
  }
  \item{distance}{
    The distance at which the function value must be considered.
    The default value is the median distance used to calculate the function values.
  }
  \item{AllowJitter}{
    If \code{TRUE}, duplicated points are jittered to avoid their elimination by the kriging procedure.
  }
  \item{Nbx, Nby}{
    The number of columns and rows (pixels) of the resulting map, 128 by default.
    Increase it for quality, paid by increasing computing time.
  }
  \item{\dots}{
    Further parameters passed to \code{\link{image}}.
  }
  \item{x}{
    An object of class "kwmppp" to plot.
  }
  \item{Contour}{
    If \code{TRUE}, contours are added to the map.
  }
  \item{Palette}{
    The color palette of the map.
  }
  \item{Contournlevels}{
    The number of levels of contours.
  }
  \item{Contourcol}{
    The color of the contour lines.
  }
  \item{CheckArguments}{
    If \code{TRUE} (default), the function arguments are verified.
    Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\value{
  \code{kmppp()} produces an object of class "kwmppp" that is also an \code{\link{autoKrige}} object that can be used to produce alternative maps.
  It can be plotted.
}
\examples{
  ReferenceType <- "V. Americana"
  NeighborType <- "Q. Rosea"
  # Calculate individual intertype M(distance) values
  fvind <- Mhat(paracou16, , ReferenceType, NeighborType, Individual=TRUE)
  # Plot the point pattern with values of M(30 meters)
  plot(kwmppp(paracou16, fvind, ReferenceType, distance=30))
  # Add the reference points to the plot
  is.ReferenceType <- paracou16$marks$PointType == ReferenceType
  points(x=paracou16$x[is.ReferenceType], y=paracou16$y[is.ReferenceType], pch=20)
}
