as.Dtable.ppp <- function (X, ...) {
  # If the ppp is not a wmppp, convert it
  if (!inherits(X, "wmppp")) X <- as.wmppp.ppp(X, ...)
  
  # Get the distance matrix
  Dmatrix <- spatstat.geom::pairdist(X)
  
  # Convert
  return(
    Dtable(
      Dmatrix, 
      PointType = spatstat.geom::marks(X)$PointType, 
      PointWeight = spatstat.geom::marks(X)$PointWeight
    )
  )
}
