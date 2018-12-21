\name{Kdhat}
\alias{Kdhat}
\title{
  Estimation of the Kd function
}
\description{
  Estimates the \emph{Kd} function
}
\usage{
 Kdhat(X, r = NULL, ReferenceType, NeighborType = ReferenceType, Weighted = FALSE,
       Original = TRUE, Approximate = ifelse(X$n < 10000, 0, 1), Adjust = 1,
       MaxRange = "ThirdW", StartFromMinR = FALSE, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked planar point pattern (\code{\link{wmppp.object}}) or a \code{\link{Dtable}} object.
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a default value is set: 512 equally spaced values are used, from the smallest distance between points to half the diameter of the window.
  }
  \item{ReferenceType}{
  One of the point types. If "", all points are considered (this is not the default value; \code{NeighborType} is ignored then) to estimate the average value of simulated \emph{Kd} values under the null hypothesis of \emph{RandomLocation} (Marcon and Puech, 2012).
  }
  \item{NeighborType}{
  One of the point types. By default, the same as reference type.
  }
  \item{Weighted}{
  Logical; if \code{TRUE}, estimates the \emph{Kemp} function.
  }
  \item{Original}{
  Logical; if \code{TRUE} (by default), the original bandwidth selection by Duranton and Overman (2005) following Silverman (1986: eq 3.31) is used. If \code{FALSE}, it is calculated following Sheather and Jones (1991), \emph{i.e.} the state of the art. See \code{\link{bw.SJ}} for more details.
  }
  \item{Approximate}{
  if not 0 (1 is a good choice), exact distances between pairs of points are rounded to 1024 times \code{Approximate} single values equally spaced between 0 and the largest distance. This technique (Scholl and Brenner, 2015) allows saving a lot of memory when addressing large point sets (the default value is 1 over 10000 points). Increasing \code{Approximate} allows better precision at the cost of proportional memory use. Ignored if \code{X} is a \code{\link{Dtable}} object.
  }
  \item{Adjust}{
  Force the automatically selected bandwidth (following \code{Original}) to be multiplied by \code{Adjust}. Setting it to values lower than one (1/2 for example) will sharpen the estimation.
  }
  \item{MaxRange}{
  The maximum value of \code{r} to consider, ignored if \code{r} is not \code{NULL}. Default is "ThirdW", one third of the diameter of the window. Other choices are "HalfW", and "QuarterW" and "D02005".
  "HalfW", and "QuarterW" are for half or the quarter of the diameter of the window.
  "D02005" is for the median distance observed between points, following Duranton and Overman (2005). "ThirdW" should be close to "DO2005" but has the advantage to be independent of the point types chosen as \code{ReferenceType} and \code{NeighborType}, to simplify comparisons between different types. "D02005" is approximated by "ThirdW" if \code{Approximate} is not 0.
  if \code{X} is a \code{\link{Dtable}} object, the diameter of the window is taken as the max distance between points.
  }
  \item{StartFromMinR}{
  Logical; if \code{TRUE}, points are assumed to be further from each other than the minimum observed distance, So \emph{Kd} will not be estimated below it: it is assumed to be 0. If \code{FALSE}, distances are smoothed down to $r=0$.
  Ignored if \code{Approximate} is not 0: then, estimation always starts from $r=0$.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{Kd} is a density, absolute measure of a point pattern structure. \emph{Kd} is computed efficiently by building a matrix of distances between point pairs and calculating the density of their distribution (the default values of \code{r} are those of the \code{\link{density}} function). The kernel estimator is Gaussian.
  
  The weighted \emph{Kd} function has been named \emph{Kemp} (\emph{emp} is for employees) by Duranton and Overman (2005).
  
  If \code{X} is not a \code{\link{Dtable}} object, the maximum value of \code{r} is obtained from the geometry of the window rather than caculating the median distance between points as suggested by Duranton and Overman (2005) to save (a lot of) calculation time.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.
}
\note{
  Estimating \emph{Kd} relies on calculating distances, exactly or approximately (if \code{Approximate} is not 0).
  Then distances are smoothed by estimating their probability density.
  Reflection is used to estimate density close to the lowest distance, that is the minimum observed distance (if \code{StartFromMinR} is \code{TRUE}) or 0: all distances below 4 times the estimation kernel bandwith apart from the lowest distance are duplicated (symmetrically with respect to the lowest distance) to avoid edge effects (underestimation of the density close to the lowest distance).
  
  Density estimation heavily relies on the bandwith.
  Starting from version 2.7, the optimal bandwith is computed from the distribution of distances between pairs of points up to twice the maximum distance considered.
  The consequence is that choosing a smaller range of distances in argument \code{r} results in less smoothed $Kd$ values.
  The default values (\code{r = NULL}, \code{MaxRange = "ThirdW"}) are such that almost all the pairs of points (except those more than 2/3 of the window diameter apart) are taken into account to determine the bandwith.
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.
  
  Marcon, E. and F. Puech (2017). A typology of distance-based measures of spatial concentration. \emph{Regional Science and Urban Economics}. 62:56-67.
  
  Scholl, T. and Brenner, T. (2015) Optimizing distance-based methods for large data sets, \emph{Journal of Geographical Systems} 17(4): 333-351.

  Sheather, S. J. and Jones, M. C. (1991) A reliable data-based bandwidth selection method for kernel density estimation. \emph{Journal of the Royal Statistical Society series B}, 53, 683-690.
  
  Silverman, B. W. (1986). \emph{Density estimation for statistics and data analysis}. Chapman and Hall, London.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{KdEnvelope}}, \code{\link{Mhat}}
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate Kd
(Paracou <- Kdhat(paracou16, , "Q. Rosea", "V. Americana"))
# Plot
plot(Paracou)
}
