rRandomLocation <-
function(X, ReferenceType = "", ReferencePoint = NULL, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  if (inherits(X, "Dtable")) {
    # Dtable case
    Index <- seq_along(marks(X)$PointType) 
    if (ReferenceType != "") {
      # Retain a single point type
      ReferencePoints <- marks(X)$PointType == ReferenceType
      # Randomize the reference points
      RandomizedReferences <- sample(Index[ReferencePoints])
      # Replace randomized elements in the index
      i <- o <- 1
      while (i <= length(marks(X)$PointType))
      {
        if (ReferencePoints[i]) {
          Index[i] <- RandomizedReferences[o]
          o <- o + 1
        }
        i <- i + 1
      }
    } else {
      Index <- sample(Index)
    }
    # Apply the randomization to PointType and PointWeight
    marks(X)$PointType <- marks(X)$PointType[Index]
    marks(X)$PointWeight <- marks(X)$PointWeight[Index]
    return(X)
  } else {
    # wmppp case
    if (!is.null(ReferencePoint)) {
      # The reference point must be < than the number of points
      if (ReferencePoint > spatstat.geom::npoints(X)) {
        stop("The number of the reference point must be smaller than the number of points in the point pattern.")
      }
      # The reference point must belong to the reference point type
      if (ReferenceType != "") {
        if (marks(X)$PointType[ReferencePoint] != ReferenceType) {
          stop("The reference point must be of the reference point type.")
        }
      }
      # Save the reference point
      ReferencePoint_ppp <- X[ReferencePoint]
      X <- X[-ReferencePoint]
    }
    if (ReferenceType != "") {
      # Retain a single point type
      X.reduced <- X[marks(X)$PointType == ReferenceType]
      RandomizedX <- rlabel(X.reduced)
    } else {
      RandomizedX <- rlabel(X)
    }
    if (!is.null(ReferencePoint)) {
      # Restore the reference point with index 1
      RandomizedX <- superimpose(ReferencePoint_ppp, RandomizedX)
    }
    class(RandomizedX) <- c("wmppp", "ppp")
    return(RandomizedX)
  }
}
