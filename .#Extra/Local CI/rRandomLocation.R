rRandomLocation <-
function(X, ReferenceType = "", ReferencePoint = NULL, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    Index <- seq_along(X$marks$PointType) 
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
    if (!is.null(ReferencePoint)) {
      # The reference point must be < than the number of points
      if (ReferencePoint > X$n) {
        stop("The number of the reference point must be smaller than the number of points in the point pattern.")
      }
      # The reference point must belong to the reference point type
      if (ReferenceType != "") {
        if (X$marks$PointType[ReferencePoint] != ReferenceType) {
          stop("The reference point must be of the reference point type.")
        }
      }
      # Save the reference point
      ReferencePoint_ppp <- X[ReferencePoint]
      X <- X[-ReferencePoint]
    }
    if (ReferenceType != "") {
      # Retain a single point type
      X.reduced <- X[X$marks$PointType == ReferenceType]
      RandomizedX <- rlabel(X.reduced)
    } else {
      RandomizedX <- rlabel(X)
    }
    if (!is.null(ReferencePoint)) {
      # Restore the reference point with index 1
      RandomizedX <- superimpose(ReferencePoint_ppp, RandomizedX)
    }
    class(RandomizedX) <- c("wmppp", "ppp")
    return (RandomizedX)
  }
}
