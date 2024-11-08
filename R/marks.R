marks.Dtable <- function (x, ...) {
  # marks method for Dtable
  return(as.data.frame(x$marks))
}

`marks<-.Dtable` <- function (x, ..., value) {
  # marks replacement method for Dtable
  y <- x
  y$marks <- value
  return(y)
}

`marks<-.wmppp` <- function(x, ..., dfok = TRUE, drop = TRUE, value) {
  Y <- spatstat.geom::`marks<-.ppp`(
    x, 
    ..., 
    dfok = TRUE, 
    drop = TRUE, 
    value = value
  )
  class(Y) <- c("wmppp", "ppp")
  return(Y)
}
