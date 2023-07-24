Smooth.wmppp <- function(X, fvind = NULL, distance = NULL, ReferenceType = "", 
                         Quantiles = FALSE, Weighted = TRUE, Adjust = 1, 
                         Nbx = 128, Nby = 128, CheckArguments = TRUE)
{
  # Check the arguments
  if (CheckArguments) {
    CheckdbmssArguments()
  }
  
  # Reduce the point pattern to the reference type
  if (ReferenceType != "") {
    is_ReferenceType <- X$marks$PointType == ReferenceType
    X <- X[is_ReferenceType]
  } 
  
  # Smooth the point weights. 
  if (is.null(fv)) {
    # Marks as a numeric vector
    X$marks <- X$marks$PointWeight
    # Smooth requires the top class of X to be ppp
    class(X) <- "ppp"
    Image <- Smooth.ppp(X, dimyx = c(Nbx, Nby))
    return(Image)
  }
  
  # Smooth the individual function values
   
  # Check the consistency between X and fvind
  if (X$n != sum(startsWith(colnames(fvind), paste0(attr(fvind, "valu"), "_"))))
    stop(paste("The number of reference points in the function value is different from \n", 
               "that of the reference points of the point pattern"))
  
  if (is.null(distance)) {
    # default distance
    distance <- stats::median(fvind$r)
  }
  # Find the max r value of fvind lower than or equal to argument distance
  r_to_plot <- max(fvind$r[fvind$r<=distance])
  # Weights
  if (Weighted) {
    weights <- X$marks$PointWeight
  } else {
    weights <- rep(1, x$n)
  }
  
  if (Quantiles) {
    # Smooth the quantiles of the dbmss
    if (is.null(attr(fvind, "Quantiles")))
      stop("The quantiles of fvind are not available.")
    # Make the quantiles the marks of X
    X$marks <- attr(fvind, "Quantiles")[
        which(rownames(attr(fvind, "Quantiles")) == r_to_plot), 
      ]
    # Smooth requires the top class of X to be ppp
    class(X) <- "ppp"
    Image <- Smooth.ppp(X, sigma = r_to_plot/2, weights = weights, adjust = Adjust, dimyx = c(Nbx, Nby))
  } else {
    # Smooth the values of the dbm
    fvind.matrix <- as.matrix(fvind)
    # Extract the values. Columns 1 to 3 contain the global dbm
    X$marks <- fvind.matrix [which(fvind.matrix [, 1] == r_to_plot), -(1:3)]
    # Smooth requires the top class of X to be ppp
    class(X) <- "ppp"
    Image <- Smooth.ppp(X, sigma = r_to_plot/2, weights = weights, adjust = Adjust, dimyx = c(Nbx, Nby))
  }
  return(Image)
}