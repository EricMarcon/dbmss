rRandomLabelingM <-
function(X, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    # Randomize marks
    X$marks$PointType <- sample(X$marks$PointType)
    return(X)
  } else {
    # wmppp case
    # Randomize marks
    RandomizedX <- rlabel(X)
    # Restore weights
    RandomizedX$marks$PointWeight <- X$marks$PointWeight
    
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
