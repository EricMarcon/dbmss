kwmppp <- function (
    X, 
    fvind, 
    ReferenceType = "", 
    distance = stats::median(fvind$r),
    AllowJitter = TRUE, 
    Nbx = 128, 
    Nby = 128, 
    CheckArguments = TRUE) {
  if (CheckArguments)
    CheckdbmssArguments()
  
  # Reduce the community to the reference type
  if (ReferenceType != "") {
    is_ReferenceType <- marks(X)$PointType == ReferenceType
    X <- X[is_ReferenceType]
  } 
  
  # Check the consistency between X and fvind
  if (
    spatstat.geom::npoints(X) != 
    sum(startsWith(colnames(fvind), paste0(attr(fvind, "valu"), "_")))
  ) {
    stop(
      paste(
        "The number of reference points in the function value is different from \n", 
        "that of the reference points of the spatialized community"
      )
    )
  }
  # Jitter
  if (AllowJitter) {
    # Find duplicates
    Dups <- spatstat.geom::duplicated.ppp(X, rule = "unmark")
    if (sum(Dups) > 0) {
      # Extract the duplicates and jitter them
      Dupswmppp <- spatstat.geom::rjitter(X[Dups])
      # Put the coordinates back into the original wmppp
      X$x[Dups] <- Dupswmppp$x
      X$y[Dups] <- Dupswmppp$y
    }
  }
  
  # Find the max r value of fvind lower than or equal to argument distance
  r_to_plot <- max(fvind$r[fvind$r <= distance])

  # Convert the data to a SpatialPointsDataFrame
  df <- as.data.frame(fvind)
  # Pivot longer. Columns are named M_1, etc. for function M
  df <- tidyr::pivot_longer(
    df,
    cols = tidyselect::starts_with(paste0(attr(fvind, "valu"),"_")),
    names_to = "point",
    values_to = "dbmss"
  )
  # Filter the appropriate distance
  df <- dplyr::filter(
    df,
    .data$r == r_to_plot
  )
  # Select the function value column only
  df <- dplyr::select(
    df,
    .data$dbmss
  )
  
  # Detect points with NA values
  is_not_na <- !is.na(df$dbmss)
  
  # Make a SpatialPointsDataFrame
  sdfCommunity <- sp::SpatialPointsDataFrame(
    coords = data.frame(x = X$x[is_not_na], y = X$y[is_not_na]),
    data = df[is_not_na,]
  )
  
  # Prepare a grid
  xy <- spatstat.geom::gridcentres(X, Nbx, Nby)
  is_inside <- spatstat.geom::inside.owin(xy$x, xy$y, X$window)
  xygrid <- sp::SpatialPoints(cbind(xy$x[is_inside], xy$y[is_inside]))
  sp::gridded(xygrid) <- TRUE
  # Proceed to krigeing
  krigedCommunity <- automap::autoKrige(dbmss~1, sdfCommunity, new_data = xygrid)
  
  # Class 
  class(krigedCommunity) <- c("kwmppp", class(krigedCommunity))
  return(krigedCommunity)
}


plot.kwmppp <- function(
    x, 
    ..., 
    Contour = TRUE, 
    Palette = grDevices::topo.colors(128, alpha = 1), 
    SuppressMargins = TRUE, 
    Contournlevels = 10, 
    Contourcol = "dark red") {
    if (SuppressMargins) {
      OldPar <- graphics::par("mar")
      graphics::par(mar = c(0, 0, 2, 0))
    }

  graphics::image(x$krige_output, col = Palette, asp = 1, ...)
  if (Contour) {
    graphics::contour(
      x$krige_output, 
      add = TRUE, 
      nlevels = Contournlevels, 
      col = Contourcol
    )
  }
  if (SuppressMargins) {
    graphics::par("mar" = OldPar)
  }
}
