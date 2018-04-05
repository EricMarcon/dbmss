\name{autoplot}
\alias{autoplot.envelope}
\title{
  ggplot methods to plot dbmss objects
}
\description{
  S3 methods for the \code{\link{autoplot}} generic.
}
\usage{
  \method{autoplot}{envelope}(object, \dots, ObsColor = "black", H0Color = "red",
        ShadeColor = "grey75", alpha=0.3, main = NULL, 
        xlab = NULL, ylab = NULL, 
        LegendLabels = c("Observed", "Expected", "Confidence\n enveloppe"))
}
\arguments{
  \item{object}{
  An object to be plotted.
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
  The opacity of the confidence envelope, between 0 and 1.
  }
  \item{main}{
  The title of the plot.
  }
  \item{xlab, ylab}{
  The axes labels.
  }
  \item{LegendLabels}{
  A vector of characters. The first two items describe the observed and null-hypothesis curves, the last item the confidence interval.
  }
}
\value{
  A \code{\link{ggplot}} object.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\examples{
data(paracou16)
# Keep only 20\% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))

# Plot the envelope (should be 1000 simulations, reduced to 20 to save time)
autoplot(KdEnvelope(X, ReferenceType="Q. Rosea", NumberOfSimulations=20))
}