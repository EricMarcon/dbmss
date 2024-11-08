sharpen.wmppp <- function(X, ...) {
  X <- sharpen.ppp(X, ...)
  class(X) <- c("wmppp", "ppp")
  return(X)
}

superimpose.wmppp <- function(...) {
  X <- superimpose.ppp(...)
  class(X) <- c("wmppp", "ppp")
  return(X)
}

unique.wmppp <- function(x, ...) {
  X <- unique.ppp(x, ...)
  class(X) <- c("wmppp", "ppp")
  return(X)
}

"[.wmppp" <- function(i, j, drop = FALSE, ..., clip = FALSE) {
  X <- "[.ppp"(i, j, drop = drop, ..., clip = clip)
  class(X) <- c("wmppp", "ppp")
  return(X)
}
