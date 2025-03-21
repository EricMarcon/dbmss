\name{GoFtest}
\alias{GoFtest}
\title{
  Goodness of Fit test between a distance based measure of spatial structure and simulations of its null hypothesis
}
\description{
  Calculates the risk to reject the null hypothesis erroneously, based on the distribution of the simulations.
}
\usage{
GoFtest(Envelope,
        Test = "DCLF",
        Scaling = "Asymmetric",
        Range = NULL)
}
\arguments{
  \item{Envelope}{
  An envelope object (\code{\link[spatstat.explore]{envelope}}) containing simulations in its \code{simfuns} attribute. It may be the result of any estimation function of the dbmss package or obtained by the \code{\link[spatstat.explore]{envelope}} function with argument \code{savefuns=TRUE}.
  }
  \item{Test}{
  A string specifying the method to summarize the deviation from the null hypothesis. The deviation may be summarized using
  "\emph{DCLF}": the integrated squared deviation is utilized, a Diggle-Cressie-Loosmore-Ford (DCLF) test is performed (default);
  "\emph{Integral}": the integrated absolute deviation is utilized;
  "\emph{MAD}": the Maximum Absolute Deviation (MAD) is utilized, a MAD test is perfomed.
  }
  \item{Scaling}{
  A string specifying the method to scale the residuals of the test. Scaling may be
  "\emph{Asymmetric}": the differences between the 2.5\% lower quantiles of simulations and the expected value is utilized to scale negative residuals, and the differences between the 2.5\% upper quantiles and the expected value is utilized to scale positive residuals (default);
  "\emph{Quantile}": ranges between the 2.5\% upper and 2.5\% lower quantiles is utilized to scale residuals, disregarding whether residuals are negative or positive;
  "\emph{Studentized}": the standard deviation of simulations is utilized;
  "\emph{None}": does not scale residuals.
  }
  \item{Range}{
  A vector of length two containing the minimum and the maximum distance over which to compute the test. If \code{NULL}, or if the selected range is outside the simulated distances, all distances in the \code{Envelope} argument are used.
  }
}
\details{
  This function regroups multiple Goodness of Fit tests: the DCLF test (Diggle 1986, Cressie 1993, Loosmore & Ford 2006, Marcon et al. 2012, Myllymäki et al. 2015), the integrated deviation test (Diggle 1979), and the MAD test (Diggle 1979, Myllymäki et al. 2015).

  Monte Carlo simulations assess how well observed distance-based measures of spatial structure align with expected measures under the null hypothesis, estimated here by the mean of simulations. For both observed and simulated measures, residuals — calculated as the differences between observed and expected values — are computed at each distance r.
  These residuals are then transformed into test-specific statistics \emph{u}: \code{Test = "MAD"} uses the maximum absolute residual; \code{Test = "Integral"}, and \code{Test = "DCLF"} use residuals to approximate the integrated deviation, and the integrated squared deviation.
  A rank test on \emph{u} evaluates the null hypothesis that the observed point pattern originates from the same point process as the simulations.

  The unequal variance of the residuals at different intervals of r influences \emph{u} statistics, and the power of Goodness of Fit tests. Myllymäki et al. (2015) proposed to scale residuals using pointwise quantiles (\code{Scaling = "Asymmetric"}, and \code{Scaling = "Quantile"}), and pointwise standard deviations (\code{Scaling = "Studentized"}).
  Goodness of Fit tests are sensitive to the distance interval over which they are performed. It is recommended to choose the distance interval to test based on \emph{a priori} hypotheses (Wiegand & Moloney 2013, Baddeley et al. 2015).
}
\value{
  A p-value.
}
\references{
  Baddeley, A., Rubak, E. and Turner, R. (2015). \emph{Spatial Point Patterns: Methodology and Applications with R}. Chapman and Hall/CRC.

  Cressie, N. A. C. (1993). \emph{Statistics for Spatial Data}. 1993 John Wiley and Sons.

  Diggle, P. J. (1979). On Parameter Estimation and Goodness-of-Fit Testing for Spatial Point Patterns. \emph{Biometrics} 35(1): 87

  Diggle, P. J. (1986). Displaced amacrine cells in the retina of a rabbit: analysis of a bivariate spatial point pattern. \emph{Journal of Neuroscience Methods} 18(1-2): 115-125.

  Loosmore, N. B. and  Ford, E. D. (2006). Statistical inference using the G or K point pattern spatial statistics. \emph{Ecology} 87(8): 1925-1931.

  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. International \emph{Journal of Ecology} 2012(Article ID 619281): 11.

  Myllymäki, M., Grabarnik, P., Seijo, H. and Stoyan, D. (2015). Deviation test construction and power comparison for marked spatial point patterns. \emph{Spatial Statistics} 11: 19-34

  Wiegand, T. and Moloney, K. A. (2013). \emph{Handbook of Spatial Point-Pattern Analysis in Ecology}. Chapman and Hall/CRC.
}
\note{
  No support exists in the literature to apply the GoF test to non-cumulative functions (\emph{g}, \emph{Kd}...).

  \code{\link{Ktest}} is a much better test (it does not rely on simulations) but it is limited to the \emph{K} function against complete spatial randomness (CSR) in a rectangle window.
}
\seealso{
  \code{\link{Ktest}}
}
\examples{

# Simulate a Matern (Neyman Scott) point pattern
nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre=c(x0, y0)))
}
X <- rNeymanScott(20, 0.2, nclust, radius=0.3, n=10)
autoplot(as.wmppp(X))

# Calculate confidence envelope (should be 1000 simulations, reduced to 50 to save time)
r <- seq(0, 0.3, 0.01)
NumberOfSimulations <- 50
Alpha <- .10
Envelope <- KEnvelope(as.wmppp(X), r, NumberOfSimulations, Alpha)
autoplot(Envelope, ./(pi*r^2) ~ r)

# DCLF test using asymmetric scaling.
# Power is correct if enough simulations are run (say >1000).
paste("p-value =", GoFtest(Envelope, Test = "DCLF",
                           Scaling = "Asymmetric", Range = c(0.1, 0.2)))

# MAD test using asymmetric scaling.
paste("p-value =", GoFtest(Envelope, Test = "MAD",
                           Scaling = "Asymmetric", Range = c(0.1, 0.2)))
}
