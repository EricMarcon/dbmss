as.Dtable.data.frame <-
function (X, ...)
{
  # Convert X to a wmppp
  X <- as.wmppp.data.frame(X, ...)
  
  # Get the distance matrix
  Dmatrix <- spatstat.geom::pairdist(X)
  
  # Convert
  return (Dtable(Dmatrix, PointType=spatstat.geom::marks(X)$PointType , PointWeight=spatstat.geom::marks(X)$PointWeight))
}
