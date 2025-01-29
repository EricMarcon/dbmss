rRandomLocation <- function(
    X,
    ReferenceType = "",
    CheckArguments = TRUE) {

  if (CheckArguments) {
    CheckdbmssArguments()
  }

  if (inherits(X, "Dtable")) {
    # Dtable case
    Index <- seq_along(marks(X)$PointType)
    if (ReferenceType != "") {
      # Retain a single point type
      ReferencePoints <- (marks(X)$PointType == ReferenceType)
      # Randomize the reference points
      RandomizedReferences <- sample(Index[ReferencePoints])
      # Replace randomized elements in the index
      i <- o <- 1
      while (i <= length(marks(X)$PointType)) {
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
    if (ReferenceType != "") {
      # Retain a single point type
      X.reduced <- X[marks(X)$PointType == ReferenceType]
      RandomizedX <- rlabel(X.reduced)
    } else {
      RandomizedX <- rlabel(X)
    }

    class(RandomizedX) <- c("wmppp", "ppp")
    return(RandomizedX)
  }
}
