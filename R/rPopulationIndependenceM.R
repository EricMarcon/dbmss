rPopulationIndependenceM <-
function(X, ReferenceType, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    ReferencePoints <- X$marks$PointType==ReferenceType
    # Index vector
    Index <- 1:length(X$marks$PointType)
    # Randomize the other points
    RandomizedOthers <- sample(Index[!ReferencePoints])
    # Replace randomized elements in the index
    i <- o <- 1
    while (i <= length(X$marks$PointType))
    {
      if (!ReferencePoints[i]) {
        Index[i] <- RandomizedOthers[o]
        o <- o+1
      }
      i <- i+1
    }
    # Apply the randomization to PointType and PointWeight
    X$marks$PointType <- X$marks$PointType[Index]
    X$marks$PointWeight <- X$marks$PointWeight[Index]
    return(X)
  } else {
    # wmppp case
    ReferencePP <- X[X$marks$PointType==ReferenceType]
    OtherPointsPP <- X[X$marks$PointType!=ReferenceType]
    RandomizedX <- superimpose(ReferencePP, rlabel(OtherPointsPP))
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
