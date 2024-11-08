Smooth.wmppp <- function(X, fvind, distance = NULL, Quantiles = FALSE, 
                         sigma = bw.scott(X, isotropic = TRUE), Weighted = TRUE, Adjust = 1, 
                         Nbx = 128, Nby = 128,..., CheckArguments = TRUE)
{
  # Check the arguments
  if (CheckArguments) {
    CheckdbmssArguments()
  }
  
  if (Quantiles) {
    # Read the risk level in fvind
    if (is.null(attr(fvind, "Alpha")))
      stop("The risk level 'Alpha' could not be read in 'fvind'. Was it computed with argument 'Quantiles = TRUE' ?")
    if (is.null(attr(fvind, "Quantiles")))
      stop("The quantiles of 'fvind' are not available. Was it computed with argument 'Quantiles = TRUE' ?")
  }

  # Read the reference type in fvind
  ReferenceType <- attr(fvind, "ReferenceType")
  if (is.null(ReferenceType))
    stop("The refence type could not be read in 'fvind'. Was it computed with argument 'Indivivual = TRUE' ?")
  
  # Reduce the point pattern to the reference type
  if (ReferenceType != "") {
    is_ReferenceType <- marks(X)$PointType == ReferenceType
    X <- X[is_ReferenceType]
  }
   
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
    weights <- marks(X)$PointWeight
  } else {
    weights <- rep(1, X$n)
  }
  
  # Read the attributes of the fvind
  if (!is.null(attr(fvind, "Alpha"))) {
    Alpha <- attr(fvind, "Alpha")
    Qvalues <- attr(fvind, "Quantiles")[which(rownames(attr(fvind, "Quantiles")) == r_to_plot), ]
  }
  
  if (Quantiles) {
    # Smooth the quantiles of the dbm
    # Make the quantiles the marks of X
    marks(X) <- Qvalues
    # Smooth() requires the top class of X to be ppp
    class(X) <- "ppp"
    # Eliminate NA's before smoothing
    is_na <- is.na(marks(X))
    weights <- weights[!is_na]
    X<- X[!is_na]
    Image <- Smooth.ppp(X, sigma = sigma, ..., weights = weights, adjust = Adjust, dimyx = c(Nby, Nbx))
  } else {
    # Smooth the values of the dbm
    fvind.matrix <- as.matrix(fvind)
    # Extract the values. Columns 1 to 3 contain the global dbm
    marks(X) <- fvind.matrix [which(fvind.matrix [, 1] == r_to_plot), -(1:3)]
    # Smooth requires the top class of X to be ppp
    class(X) <- "ppp"
    # Eliminate NA's before smoothing
    is_na <- is.na(marks(X))
    weights <- weights[!is_na]
    X<- X[!is_na]
    Image <- Smooth.ppp(X, sigma = sigma, ..., weights = weights, adjust = Adjust, dimyx = c(Nby, Nbx))
  }
  # Statistical significance saved in attributes
  if (!is.null(attr(fvind, "Alpha"))) {
    # Eliminate NAs to obtain FALSE in attributes High and Low
    Qvalues[is.na(Qvalues)] <- 0.5
    attr(Image, "High") <- Qvalues >= 1 - Alpha / 2
    attr(Image, "Low") <- Qvalues <= Alpha / 2
  }
  return(Image)
}