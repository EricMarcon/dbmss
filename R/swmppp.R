swmppp <-
  function (X, fvind, ReferenceType = "", distance = stats::median(fvind$r), 
            AllowJitter = TRUE, Nbx = 128, Nby = 128,
            Adjust = 1, CheckArguments = TRUE)
{
  if (CheckArguments)
    CheckdbmssArguments()
  
  # Reduce the community to the reference type
  if (ReferenceType != "") {
    is_ReferenceType <- X$marks$PointType == ReferenceType
    X <- X[is_ReferenceType]
  } 
  
  # Check the consistency between X and fvind
  if (X$n != sum(startsWith(colnames(fvind), paste0(attr(fvind, "valu"), "_"))))
    stop(paste("The number of reference points in the function value is different from \n", 
               "that of the reference points of the spatialized community"))
  
  # Jitter
  if (AllowJitter) {
    # Find duplicates
    Dups <- spatstat.geom::duplicated.ppp(X, rule="unmark")
    if (sum(Dups)>0) {
      # Extract the duplicates and jitter them
      Dupswmppp <- spatstat.geom::rjitter(X[Dups])
      # Put the coordinates back into the original wmppp
      X$x[Dups] <- Dupswmppp$x
      X$y[Dups] <- Dupswmppp$y
    }
  }
  
  # Find the max r value of fvind lower than or equal to argument distance
  r_to_plot <- max(fvind$r[fvind$r<=distance])
  
  # Format the value to plot
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
  # Make it the marks of X
  X$marks <- df
  
  # Smoothed value of the dbmss. 
  # Bandwidth is r/2 so that 95% of the weight is each point's neighborhood
  return(spatstat.explore::Smooth.ppp(X, sigma = r_to_plot/2, adjust = Adjust))
}    
