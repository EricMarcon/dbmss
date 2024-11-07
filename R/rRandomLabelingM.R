rRandomLabelingM <-
function(X, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    # Randomize marks
    spatstat.geom::marks(X)$PointType <- sample(spatstat.geom::marks(X)$PointType)
    return(X)
  } else {
    # wmppp case
    # Randomize marks
    RandomizedX <- rlabel(X)
    # Restore weights
    marks(RandomizedX)$PointWeight <- spatstat.geom::marks(X)$PointWeight
    
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
