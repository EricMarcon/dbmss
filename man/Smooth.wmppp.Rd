\name{Smooth.wmppp}
\alias{Smooth.wmppp}
\title{Spatial smoothing of individual dbmss's}
\usage{
  \method{Smooth}{wmppp} (X, fvind, distance = NULL, Quantiles = FALSE, 
      sigma = bw.scott(X, isotropic = TRUE), Weighted = TRUE, Adjust = 1, 
      Nbx = 128, Nby = 128, \dots, CheckArguments = TRUE)
}
\description{
  Performs spatial smoothing of the individual values of distance-based measures computed in the neighborhood of each point.
}
\arguments{
  \item{X}{
    A point pattern (\code{\link{wmppp.object}}).
  }
  \item{fvind}{
    An object of class \code{fv}, see \code{\link{fv.object}}, obtained a distance-based method, such as \code{\link{Mhat}} with individual values (argument \code{Individual = TRUE}).
  }
  \item{distance}{
    The distance at which the function value must be considered.
    The default value is the median distance used to calculate the function values.
  }
  \item{Quantiles}{
    If \code{FALSE} (default), the dbmss is smoothed to produce a map of the measure.
    If \code{TRUE}, its quantiles (computed by \code{\link{Mhat}} with argument \code{Quantiles = TRUE}) are smoothed to produce a map of the confidence level of the measure.
  }
  \item{Weighted}{
    If \code{TRUE} (default), the point weights are taken into account for smoothing.
  }
  \item{sigma}{
  The bandwidth used for smoothing.
  A Gaussian kernel is used (see \code{\link{Smooth.ppp}}).
  Its bandwidth is chosen by default according to Scott's rule (see \code{\link{bw.scott}}).
  }
  \item{Adjust}{
  Force the selected bandwidth (\code{sigma}) to be multiplied by \code{Adjust}. Setting it to values smaller than one (1/2 for example) will sharpen the estimation. 
  }
  \item{Nbx, Nby}{
    The number of columns and rows (pixels) of the resulting map, 128 by default.
    Increase it for quality, paid by increasing computing time.
  }
  \item{\dots}{
    Extra arguments, passed to \code{\link{Smooth.ppp}}.
  }
  \item{CheckArguments}{
    If \code{TRUE} (default), the function arguments are verified.
    Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\value{
  An image that can be plotted.
  If quantiles have been computed in \code{fvind}, attributes "High" and "Low" contain logical vectors to indentify significantly high and low quantiles.
}
\examples{
  ReferenceType <- "V. Americana"
  NeighborType <- "Q. Rosea"
  # Calculate individual intertype M(distance) values
  fvind <- Mhat(paracou16, r=c(0, 30), ReferenceType, NeighborType, Individual=TRUE)
  # Plot the point pattern with values of M(30 meters)
  p16_map <- Smooth(paracou16, fvind, distance=30)
  plot(p16_map, main = "")
  # Add the reference points to the plot
  is.ReferenceType <- paracou16$marks$PointType == ReferenceType
  points(x=paracou16$x[is.ReferenceType], y=paracou16$y[is.ReferenceType], pch=20)
  # Add contour lines
  contour(p16_map, nlevels = 5, add = TRUE)
}
