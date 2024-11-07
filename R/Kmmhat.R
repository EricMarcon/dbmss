Kmmhat <-
function(X, r = NULL, ReferenceType = "", CheckArguments = TRUE) {

  if (CheckArguments)
    CheckdbmssArguments()
  
  # KmmBymarkcorrint calls Kmark (previously called markcorrint) with the best edge-effect correction and returns the values
  KmmBymarkcorrint <- function (X, r) {
    X.marked <- X
    # Weights are normalized so that their mean is 1 because markcorrint returns Kmm * mean weight instead of Kmm (as of v. 1.27-0 of spatstat).
    spatstat.geom::marks(X.marked) <- spatstat.geom::marks(X)$PointWeight/mean(spatstat.geom::marks(X)$PointWeight)
    Kmm <- spatstat.explore::Kmark(X.marked, correction="best")
    attr(Kmm, "ylab") <- attr(Kmm, "yexp") <- quote(K[mm](r))
    attr(Kmm, "fname") <- "K[mm]"
    return (Kmm)
  } 
  
  # Kmm all points or specified point type
  if (ReferenceType == "") {
    return (KmmBymarkcorrint(X, r))
    } else {
    X.reduced <- X[spatstat.geom::marks(X)$PointType == ReferenceType]
    return (KmmBymarkcorrint(X.reduced, r))
  }   
}
