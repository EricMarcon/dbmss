rPopulationIndependenceM <-
function(X, ReferenceType, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    ReferencePoints <- marks(X)$PointType==ReferenceType
    # Index vector
    Index <- seq_along(marks(X)$PointType)
    # Randomize the other points
    RandomizedOthers <- sample(Index[!ReferencePoints])
    # Replace randomized elements in the index
    i <- o <- 1
    while (i <= length(marks(X)$PointType))
    {
      if (!ReferencePoints[i]) {
        Index[i] <- RandomizedOthers[o]
        o <- o+1
      }
      i <- i+1
    }
    # Apply the randomization to PointType and PointWeight
    marks(X)$PointType <- marks(X)$PointType[Index]
    marks(X)$PointWeight <- marks(X)$PointWeight[Index]
    return(X)
  } else {
    # wmppp case
    ReferencePP <- X[marks(X)$PointType==ReferenceType]
    OtherPointsPP <- X[marks(X)$PointType!=ReferenceType]
    RandomizedX <- superimpose(ReferencePP, rlabel(OtherPointsPP))
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
