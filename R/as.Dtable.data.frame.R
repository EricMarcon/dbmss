as.Dtable.data.frame <- function (X, ...) {
  # Convert X to a wmppp
  X <- as.wmppp.data.frame(X, ...)
  
  # Get the distance matrix
  Dmatrix <- spatstat.geom::pairdist(X)
  
  # Convert
  return (
    Dtable(
      Dmatrix, 
      PointType = marks(X)$PointType, 
      PointWeight = marks(X)$PointWeight
    )
  )
}
