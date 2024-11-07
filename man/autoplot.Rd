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
        MaxPointTypes = 6, Other = "Other",
        main = NULL, xlab = NULL, ylab = NULL, LegendLabels = NULL, 
        labelSize = "Weight", labelColor = "Type", palette="Set1",
        windowColor = "black", windowFill = "transparent", alpha = 1)
}
\arguments{
  \item{object}{
  An object to be plotted.
  }
  \item{fmla}{
  An R language formula determining which variables or expressions are plotted. Either a formula object, or a string that can be parsed as a formula. See \code{\link[spatstat.explore]{plot.fv}}.
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
  The opacity of the confidence envelope (in function values) or the points (in maps), between 0 and 1.
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
  \item{MaxPointTypes}{
  The maximum number of different point types to show. If the point set contains more of them, the less frequent ones are gathered as "Other". This number must be limited for readability and not to exceed the number of colors offered by the palette.
  }
  \item{Other}{
  The name of the point types gathered as "Other".
  }
  \item{labelSize}{
  The guide of the point size legend in point maps, i.e. what the `PointSize` mark represents. 
  }
  \item{labelColor}{
  The guide of the point color legend in point maps, i.e. what the `PointType` mark represents. 
  }
  \item{palette}{
  The color palette used to display point types in maps. See \code{\link[ggplot2]{scale_color_brewer}}
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
  A \code{\link[ggplot2]{ggplot}} object.
}
\author{
  Eric Marcon <Eric.Marcon@agroparistech.fr>, parts of the code from \code{spatstat.explore::plot.fv}.
}
\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))
autoplot(X)

# Plot the envelope (should be 1000 simulations, reduced to 20 to save time)
autoplot(KdEnvelope(X, ReferenceType="Q. Rosea", NumberOfSimulations=20))

# With a formula and a compact legend
autoplot(KEnvelope(X, NumberOfSimulations=20), 
    ./(pi*r^2) ~ r,
    LegendLabels=c("Observed", "Expected", "Confidence\n enveloppe"))
}
