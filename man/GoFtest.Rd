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
        Scaling = "Asymmetric",
        Distance = "DCLF",
        Range = NULL)
}
\arguments{
  \item{Envelope}{
  An envelope object (\code{\link[spatstat.explore]{envelope}}) containing simulations in its \code{simfuns} attribute. It may be the result of any estimation function of the dbmss package or obtained by the \code{\link[spatstat.explore]{envelope}} function with argument \code{savefuns=TRUE}.
  }
  \item{Scaling}{
  A string specifying the method to scale the residuals of the test. Scaling may be
  "\emph{Asymmetric}": the differences between the 2.5\% lower quantiles of simulations and the expected value is utilized to scale negative residuals, and the differences between the 2.5\% upper quantiles and the expected value is utilized to scale positive residuals (default);
  "\emph{Quantile}": ranges between the 2.5\% upper and 2.5\% lower quantiles is utilized to scale residuals, disregarding whether they are negative or positive;
  "\emph{Studentized}": the standard deviation of simulations is utilized;
  "\emph{None}": does not scale residuals.
  }
  \item{Distance}{
  A string specifying the method to summarize the deviation from the null hypothesis. The deviation may be summarized using
  "\emph{DCLF}": the integrated squared deviation is utilized, a Diggle-Cressie-Loosmore-Ford (DCLF) test is performed (default);
  "\emph{Integral}": the integrated absolute deviation is utilized;
  "\emph{MAD}": the Maximum Absolute Deviation (MAD) is utilized, a MAD test is perfomed.
  }
  \item{Range}{
  A vector of length two containing the minimum and the maximum distance over which to compute the test. If \code{NULL}, or if the selected range is outside the simulated distances, all distances in the \code{Envelope} argument are used.
  }
}
\details{
  This test was introduced by Diggle(1983) and extensively developped by Loosmore and Ford (2006) for \emph{K}, and applied to \emph{M} by Marcon et al. (2012).
}
\value{
  A p-value.
}
\references{
  Diggle, P. J. (1983). \emph{Statistical analysis of spatial point patterns}. Academic Press, London. 148 p.

  Loosmore, N. B. and  Ford, E. D. (2006). Statistical inference using the G or K point pattern spatial statistics. \emph{Ecology} 87(8): 1925-1931.

  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. International \emph{Journal of Ecology} 2012(Article ID 619281): 11.
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

# GoF test. Power is correct if enough simulations are run (say >1000).
paste("p-value =", GoFtest(Envelope))
}
