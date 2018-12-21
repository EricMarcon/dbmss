\name{mhat}
\alias{mhat}
\title{
  Estimation of the m function
}
\description{
  Estimates the \emph{m} function
}
\usage{
mhat(X, r = NULL, ReferenceType, NeighborType = ReferenceType,
    CaseControl = FALSE, Original = TRUE, Approximate = ifelse(X$n < 10000, 0, 1),
    Adjust = 1, MaxRange = "ThirdW", Individual = FALSE, CheckArguments = TRUE)
}
\arguments{
  \item{X}{
  A weighted, marked planar point pattern (\code{\link{wmppp.object}}) or a \code{\link{Dtable}} object.
  }
  \item{r}{
  A vector of distances. If \code{NULL}, a default value is set: 512 equally spaced values are used, from the smallest distance to the range defined by \code{MaxRange}. the between points to half the diameter of the window.
  }
  \item{ReferenceType}{
  One of the point types.
  }
  \item{NeighborType}{
  One of the point types. By default, the same as reference type.
  }
  \item{CaseControl}{
  Logical; if \code{TRUE}, the case-control version of \emph{M} is computed. \emph{ReferenceType} points are cases, \emph{NeighborType} points are controls.
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
  If \code{X} is a \code{\link{Dtable}} object, the diameter of the window is taken as the max distance between points.
  }
  \item{Individual}{
  Logical; if \code{TRUE}, values of the function around each individual point are returned.
  }
  \item{CheckArguments}{
  Logical; if \code{TRUE}, the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
  }
}
\details{
  \emph{m} is a weighted, density, relative measure of a point pattern structure (Lang \emph{et al.}, 2014). Its value at any distance is the ratio of neighbors of the \emph{NeighborType} to all points around \emph{ReferenceType} points, normalized by its value over the windows.
  
  The number of neighbors at each distance is estimated by a Gaussian kernel whose bandwith is chosen optimally according to Silverman (1986: eq 3.31). It can be sharpened or smoothed by multiplying it by \code{Adjust}. The bandwidth of Sheather and Jones (1991) would be better but it is very slow to calculate for large point patterns and it sometimes fails. It is often sharper than that of Silverman. 
  
  If \code{X} is not a \code{\link{Dtable}} object, the maximum value of \code{r} is obtained from the geometry of the window rather than caculating the median distance between points as suggested by Duranton and Overman (2005) to save (a lot of) calculation time.
  
  If \emph{CaseControl} is \code{TRUE}, then \emph{ReferenceType} points are cases and \emph{NeighborType} points are controls. The univariate concentration of cases is calculated as if \emph{NeighborType} was equal to \emph{ReferenceType}, but only controls are considered when counting all points around cases (Marcon et al., 2012). This makes sense when the sampling design is such that all points of \emph{ReferenceType} (the cases) but only a sample of the other points (the controls) are recorded. Then, the whole distribution of points is better represented by the controls alone.
}
\value{
  An object of class \code{fv}, see \code{\link{fv.object}}, which can be plotted directly using \code{\link{plot.fv}}.

  If \code{Individual} is set to \code{TRUE}, the object also contains the value of the function around each individual \emph{ReferenceType} point taken as the only reference point. The column names of the \code{fv} are "m_" followed by the point names, i.e. the row names of the marks of the point pattern.
}
\note{
  Estimating \emph{m} relies on calculating distances, exactly or approximately (if \code{Approximate} is not 0).
  Then distances are smoothed by estimating their probability density.
  In contrast with \code{\link{Kdhat}}, reflection is not used to estimate density close to the lowest distance.
  The same kernel estimation is applied to the distances from reference points of neighbor points and of all points.
  Since \emph{m} is a relative function, a ratio of densities is calculated, that makes the features of the estimation vanish.
  
  Density estimation heavily relies on the bandwith.
  Starting from version 2.7, the optimal bandwith is computed from the distribution of distances between pairs of points up to twice the maximum distance considered.
  The consequence is that choosing a smaller range of distances in argument \code{r} results in less smoothed $m$ values.
  The default values (\code{r = NULL}, \code{MaxRange = "ThirdW"}) are such that almost all the pairs of points (except those more than 2/3 of the window diameter apart) are taken into account to determine the bandwith.
}
\references{
  Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using Micro-Geographic Data. \emph{Review of Economic Studies} 72(4): 1077-1106.

  Lang G., Marcon E. and Puech F. (2014) Distance-Based Measures of Spatial Concentration: Introducing a Relative Density Function. \emph{HAL} 01082178, 1-18.

  Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative spatial structure of point patterns. \emph{International Journal of Ecology} 2012(Article ID 619281): 11.

  Scholl, T. and Brenner, T. (2015) Optimizing distance-based methods for large data sets, \emph{Journal of Geographical Systems} 17(4): 333-351.

  Sheather, S. J. and Jones, M. C. (1991) A reliable data-based bandwidth selection method for kernel density estimation. \emph{Journal of the Royal Statistical Society series B}, 53, 683-690.

  Silverman, B. W. (1986). \emph{Density estimation for statistics and data analysis}. Chapman and Hall, London.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>
}
\seealso{
  \code{\link{mEnvelope}}, \code{\link{Kdhat}}
}
\examples{
data(paracou16)
plot(paracou16)

# Calculate M
plot(mhat(paracou16, , "V. Americana", "Q. Rosea"))
}
