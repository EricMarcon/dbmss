Dtable <-
function(Dmatrix, PointType = NULL, PointWeight = NULL) 
{
  # Dmatrix must be a distance matrix
  if (is.matrix(Dmatrix)) {
    if (nrow(Dmatrix) != ncol(Dmatrix))
      stop("Dmatrix should be a square matrix.")
    if (any(is.na(Dmatrix)))
      stop("NAs are not allowed in the distance matrix.")
    if (any(Dmatrix < 0))
      stop("negative values are not allowed in the distance matrix.")
    if (any(diag(Dmatrix) > 0))
      stop("diagonal values of the distance matrix must be 0.")
  } else {
    stop("Dmatrix must be a matrix.")
  }

  # Get PointType
  if (is.null(PointType)) {
    # Point types should be in the row or column names of the matrix
    if (!is.null(rownames(Dmatrix))) {
      PointType <- rownames(Dmatrix)
      if (!is.null(colnames(Dmatrix))) {
        # Check row and col names are identical
        if (colnames(Dmatrix) != rownames(Dmatrix))
          stop("row and column names of the distance matrix are different.")
      } 
    } else {
      if (!is.null(colnames(Dmatrix)))
        PointType <- colnames(Dmatrix)
    }
  }

  # Check PointType
  if (any(is.null(PointType)))
    stop("NULL values are not allowed in the point types.")
  if (any(is.na(PointType)))
    stop("NAs are not allowed in the point types.")
  if (length(PointType) != nrow(Dmatrix))
    stop("The vector of point types must have the same size as Dmatrix.")
  PointType <- as.factor(PointType)

  # Get PointWeight
  if (is.null(PointWeight)) {
    PointWeight <- rep(1, length(PointType))
  } else {
    if (any(is.na(PointWeight)))
      stop("NAs are not allowed in the point weights.")
    if (any(PointWeight <= 0))
      stop("Point weights must be strictly positive.")
    if (length(PointWeight) != nrow(Dmatrix))
      stop("The vector of point weights must have the same size as Dmatrix.")
  }
  
  # Build the object
  Dt <- list(Dmatrix=Dmatrix, 
             n=nrow(Dmatrix), 
             marks=list(PointType=PointType, PointWeight=PointWeight)
             )
  class(Dt) <- "Dtable"
  return (Dt)
}
