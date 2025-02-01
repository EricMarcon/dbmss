ghat <- function(
    X,
    r = NULL,
    ReferenceType = "",
    NeighborType = "",
    CheckArguments = TRUE) {

  if (CheckArguments) {
    CheckdbmssArguments()
    # Eliminate erroneous configurations
    if ((ReferenceType == "" | NeighborType == "") & (ReferenceType != NeighborType)) {
      stop("Either two or no point type must be specified.")
    }
  }

  area <- area.owin(X$window)
  autor <- is.null(r)

  # Calculate densities
  # g intra
  if (ReferenceType == "" & NeighborType == "") {
    lambdaI <- lambdaJ <- X$n / area
  } else {
    # g intra for a single point type
    if (ReferenceType == NeighborType) {
      X.reduced <- X[marks(X)$PointType == ReferenceType]
      lambdaI <- lambdaJ <- X.reduced$n / area
    }
    # g inter
    if (ReferenceType != NeighborType) {
      X.cross <- X[marks(X)$PointType == ReferenceType]
      Y.cross <- X[marks(X)$PointType == NeighborType]
      lambdaI <- X.cross$n / area
      lambdaJ <- Y.cross$n / area
    }
  }

  # Find the max value of r
  if (is.null(r)) {
    rmax <- rmax.rule(fun = "K", W = X$window, lambda = lambdaJ)
  } else {
    rmax <- max(r)
  }

  # Find the best breaks
  breaks <- handle.r.b.args(window = X$window, rmaxdefault = rmax)
  rBest <- breaks$r
  if (is.null(r)) {
    # set default r
    r <- rBest
  }


  # Bandwidth (Stoyan and Stoyan, 1994, pages 284–285)
  stoyan <- 0.15
  hmax <- stoyan / sqrt(lambdaJ)
  bw <- hmax / sqrt(5)

  # Find pairs
  if (ReferenceType == "" & NeighborType == "") {
    # g intra
    Pairs <- closepairs(X, rmax = rmax + hmax)
  } else if (ReferenceType == NeighborType) {
    # g intra for a single point type
    Pairs <- closepairs(X.reduced, rmax = rmax + hmax)
  } else {
    # g inter
    Pairs <- crosspairs(X = X.cross, Y = Y.cross, rmax = rmax + hmax)
  }

  # Adapted from pcf.ppp {spatstat}

  # Geometry
  XI <- ppp(Pairs$xi, Pairs$yi, window = X$window, check = FALSE)
  XJ <- ppp(Pairs$xj, Pairs$yj, window = X$window, check = FALSE)

  # Edge-effect correction
  if (is.rectangle(X$window) |  is.polygonal(X$window)) {
    edgewt <- edge.Ripley(XI, r = matrix(Pairs$d, ncol = 1))
    valu <- "iso"
    desc <- "Ripley isotropic correction estimate of %s"
  } else {
    edgewt <- edge.Trans(X = XI, Y = XJ, paired = TRUE)
    valu <- "trans"
    desc <- "translation-corrected estimate of %s"
  }

  # Estimate g
  gEstimate <- sewpcf(
    Pairs$d,
    w = edgewt,
    denargs = spatstat.utils::resolve.defaults(
      list(
        kernel = "epanechnikov",
        bw = bw,
        n = length(rBest),
        from = 0,
        to = max(rBest)
      )
    ),
    lambda2area = lambdaI * lambdaJ * area
  )

  # Calculate values for r if specified
  if (!autor) {
    g <- stats::approx(
      x = gEstimate$r,
      y = gEstimate$g,
      xout = r
    )$y
    gEstimate <- data.frame(r, g)
  }

  # Add theoretical value
  theo <- rep(1, length(r))
  gEstimate <- data.frame(gEstimate[, 1], theo, gEstimate[, 2])
  ColNames <- c("r", "theo", valu)
  colnames(gEstimate) <- ColNames

  # Return the values of g(r)
  g <- fv(
    gEstimate,
    argu = "r",
    ylab = quote(g(r)),
    valu = valu,
    fmla = ". ~ r",
    alim = c(0, max(r)),
    labl = c("r", "%s[pois](r)", paste("hat(%s)[", valu, "](r)", sep = "")),
    desc = c("distance argument r", "theoretical Poisson %s", desc),
    unitname = X$window$unit,
    fname = "g"
  )
  fvnames(g, ".") <- ColNames[-1]
  return(g)
}
