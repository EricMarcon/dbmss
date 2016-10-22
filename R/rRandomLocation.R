rRandomLocation <-
function(X, ReferenceType = "", CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    Index <- 1:length(X$marks$PointType) 
    if (ReferenceType != "") {
      # Retain a single point type
      ReferencePoints <- X$marks$PointType==ReferenceType
      # Randomize the reference points
      RandomizedReferences <- sample(Index[ReferencePoints])
      # Replace randomized elements in the index
      i <- o <- 1
      while (i <= length(X$marks$PointType))
      {
        if (ReferencePoints[i]) {
          Index[i] <- RandomizedReferences[o]
          o <- o+1
        }
        i <- i+1
      }
    } else {
      Index <- sample(Index)
    }
    # Apply the randomization to PointType and PointWeight
    X$marks$PointType <- X$marks$PointType[Index]
    X$marks$PointWeight <- X$marks$PointWeight[Index]
    return(X)
  } else {
    # wmppp case
    if (ReferenceType != "") {
      # Retain a single point type
      X.reduced <- X[X$marks$PointType == ReferenceType]
      RandomizedX <- rlabel(X.reduced)
    } else {
      RandomizedX <- rlabel(X)
    }
    
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
