wmppp <-
function(df, window = NULL, unitname = NULL) 
{
  # Check the data
  if (!is.data.frame(df))
    stop("The data used to create a wmppp must be a dataframe.")
  if (tibble::is_tibble(df)) # Tibbles must be coerced to data.frames or as.numeric below won't work.
    df <- as.data.frame(df)
  if (ncol(df) < 2)
    stop("The data used to create a wmppp must have at least two columns for coordinates X and Y.")
  names(df) <- tolower(names(df))
  # Read X and Y
  if ("x" %in% names(df) & "y" %in% names(df)) {
    X <- as.numeric(df[, "x"])
    Y <- as.numeric(df[, "y"])
  } else {
    warning("No columns named X and Y have been found. Columns #1 and #2 have been used for coordinates.")
    X <- as.numeric(df[, 1])
    Y <- as.numeric(df[, 2])
  }
  if (!is.numeric(c(X, Y)))
    stop("Point coordinates X and Y must be numeric.")
  
  # Read Point Types
  if ("pointtype" %in% names(df)) {
    PointType <- df[, "pointtype"]
  } else {
    if (ncol(df) < 3) {
      warning("No column has been found for PointType. All point types have been set to All.")      
      PointType <- as.factor(rep("All", length(X)))
    } else {
      warning("No column named PointType has been found. Columns #3 has been used for labels.")
      PointType <- df[, 3]
    }
  }
  if (!is.factor(PointType)) {
    if (is.character(PointType)) {
      PointType <- as.factor(PointType)
    } else {
      stop("Point types must be factors or characters.")
    }
  }
  
  # Read Point Weights
  if ("pointweight" %in% names(df)) {
    PointWeight <- df[, "pointweight"]
  } else {
    if (ncol(df) < 4) {
      warning("No column has been found for PointWeight. All point weights have been set to 1.")      
      PointWeight <- rep(1, length(X))
    } else {
      warning("No column named PointWeight has been found. Columns #4 has been used for weights.")
      PointWeight <- df[, 4]
    }
  }
  if (!is.numeric(PointWeight))
    stop("Point weights must be numeric.")
  if (any(PointWeight < 0))
    stop("Point weights must be positive.")
  
  if (!is.null(window))
    if (!is.owin(window))
      stop("window must be an object of class owin.")

  # Full window: keep all points for their names
  w <- owin(xrange=c(min(X), max(X)), yrange=c(min(Y), max(Y)), unitname=unitname)
  
  # Build the object
  wmpppX <- ppp(X, Y, window=w, marks=data.frame(PointWeight, PointType))
  # Keep the point names
  if ("pointname" %in% names(df)) {
    row.names(wmpppX$marks) <- df[, "pointname"]
  } else {
    row.names(wmpppX$marks) <- row.names(df)
  }
  
  # Crop the window
  if (is.null(window)) {
    warning("No window has been specified. A rectangle window containing all points has been used.")
  } else {
    wmpppX <- wmpppX[window]
  }
  
  class(wmpppX) <- c("wmppp", "ppp")
  return (wmpppX)
}
