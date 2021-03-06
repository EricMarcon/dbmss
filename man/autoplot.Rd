\name{autoplot}
\alias{autoplot.envelope}
\alias{autoplot.fv}
\alias{autoplot.wmppp}
\title{
  ggplot methods to plot dbmss objects
}
\description{
  S3 methods for the \code{\link{autoplot}} generic.
}
\usage{
  \method{autoplot}{envelope}(object, fmla, \dots, ObsColor = "black", 
        H0Color = "red", ShadeColor = "grey75", alpha = 0.3, main = NULL, 
        xlab = NULL, ylab = NULL, LegendLabels = NULL)
  \method{autoplot}{fv}(object, fmla, \dots, ObsColor = "black", 
        H0Color = "red", ShadeColor = "grey75", alpha = 0.3, main = NULL, 
        xlab = NULL, ylab = NULL, LegendLabels = NULL)
  \method{autoplot}{wmppp}(object, \dots, show.window = TRUE, 
        ignore.types = length(unique(object$marks$PointType)) > 9,
        main = NULL, xlab = NULL, ylab = NULL, LegendLabels = NULL, 
        labelSize = "Weight", labelColor = "Type", palette="Set1",
        windowColor = "black", windowFill = "transparent", alpha = 0)
}
\arguments{
  \item{object}{
  An object to be plotted.
  }
  \item{fmla}{
  An R language formula determining which variables or expressions are plotted. Either a formula object, or a string that can be parsed as a formula. See \code{\link[spatstat.core]{plot.fv}}.
  }
  \item{\dots}{
  Extra arguments, currently unused.
  }
  \item{ObsColor}{
  The color of the line representing observed values of the function.
  }
  \item{H0Color}{
  The color of the line representing the null hypothesis values of the function.
  }
  \item{ShadeColor}{
  The color of the confidence envelope.
  }
  \item{alpha}{
  The opacity of the confidence envelope (in function values) or the window (in maps), between 0 and 1.
  }
  \item{main}{
  The title of the plot.
  }
  \item{xlab, ylab}{
  The axes labels.
  }
  \item{LegendLabels}{
  A vector of characters. The first two items describe the observed and null-hypothesis curves, the third and last item the confidence interval.
  To be used only in plots with two curves (typically observed and expected values).
  The default is `NULL` to display the full description of functions.
  }
  \item{show.window}{
  if `TRUE`, the borders of the window containing the points are shown on the point map.
  }
  \item{ignore.types}{
  if `TRUE`, all points are seen as belonging to the same type. Useful when too many point types prevent the plot from being readable. By default, the limit is 9 point types.
  }
  \item{labelSize}{
  The guide of the point size legend in point maps, i.e. what the `PointSize` mark represents. 
  }
  \item{labelColor}{
  The guide of the point color legend in point maps, i.e. what the `PointType` mark represents. 
  }
  \item{palette}{
  The color palette used to display point types in maps. See \code{\link[ggplot2]{scale_colour_brewer}}
  }
  \item{windowColor}{
  The color used to draw the limits of the windows in point maps. 
  }
  \item{windowFill}{
  The color used to fill the windows in point maps. 
  }
}
\details{
  Plots of `wmppp` objects are a single representation of both point types and point weights.
  Rectangular and polygonal windows (see \code{\link[spatstat.geom]{owin.object}}) are supported but mask windows are ignored (use the `plot` method if necessary).
}
\value{
  A \code{\link{ggplot}} object.
}
\author{
  Eric Marcon <Eric.Marcon@agroparistech.fr>, parts of the code from \code{spatstat.core::plot.fv}.
}
\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))

# Plot the envelope (should be 1000 simulations, reduced to 20 to save time)
autoplot(KdEnvelope(X, ReferenceType="Q. Rosea", NumberOfSimulations=20))

# With a formula and a compact legend
autoplot(KEnvelope(X, NumberOfSimulations=20), 
    ./(pi*r^2) ~ r,
    LegendLabels=c("Observed", "Expected", "Confidence\n enveloppe"))
}
