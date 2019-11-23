rRandomPositionK <-
function(X, Precision = 0, CheckArguments = TRUE) {
  
  if (CheckArguments)
    CheckdbmssArguments()
  
  # Draw in a binomial process
  RandomizedX <- runifpoint(X$n, win=X$window)
  
  # Precision
  if (Precision) {
    RandomizedX$x <- round(RandomizedX$x / Precision) * Precision
    RandomizedX$y <- round(RandomizedX$y / Precision) * Precision
  }
  
  # Apply original marks to new points
  marks(RandomizedX) <- data.frame(PointWeight=X$marks$PointWeight, PointType=X$marks$PointType)
  class(RandomizedX) <- c("wmppp", "ppp")
  return (RandomizedX)
}
