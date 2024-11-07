rRandomLabelingM <-
function(X, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    # Randomize marks
    marks(X)$PointType <- sample(marks(X)$PointType)
    return(X)
  } else {
    # wmppp case
    # Randomize marks
    RandomizedX <- rlabel(X)
    # Restore weights
    marks(RandomizedX)$PointWeight <- marks(X)$PointWeight
    
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
