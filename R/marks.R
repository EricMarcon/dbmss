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