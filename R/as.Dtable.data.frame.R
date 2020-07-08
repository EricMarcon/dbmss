as.Dtable.data.frame <-
function (X, ...)
{
  # Convert X to a wmppp
  X <- as.wmppp.data.frame(X, ...)
  
  # Get the distance matrix
  Dmatrix <- spatstat.core::pairdist(X)
  
  # Convert
  return (Dtable(Dmatrix, PointType=X$marks$PointType , PointWeight=X$marks$PointWeight))
}
