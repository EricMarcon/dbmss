Mhat <-
function(X, r = NULL, ReferenceType, NeighborType = ReferenceType, 
         CaseControl = FALSE, Individual = FALSE, ReferencePoint = NULL,
         Quantiles = FALSE, NumberOfSimulations = 100, Alpha = 0.05, 
         verbose = interactive(), CheckArguments = TRUE)
{
  # Eliminate erroneous configurations
  if (CheckArguments) {
    CheckdbmssArguments()
    if (CaseControl & (ReferenceType==NeighborType)) {
      warning("Cases and controls are identical.")
      return(rep(1,length(r)))
    }
    if (Quantiles & !Individual)
      stop("Quantiles can't be TRUE if Individual is FALSE.")
  }
  
  # Default r values: 64 values up to half the max distance
  if (is.null(r)) {
    if (inherits(X, "Dtable")) {
      # Dtable case
      rMax <- max(X$Dmatrix)
    } else {
      # wmppp case
      rMax <- diameter(X$window)
    }
    r <- rMax*c(0, 1:20, seq(22, 40, 2), seq(45, 100,5), seq(110, 200, 10), seq(220, 400, 20))/800
  }
  
  # Vectors to recognize point types
  IsReferenceType <- X$marks$PointType==ReferenceType
  IsNeighborType <- X$marks$PointType==NeighborType
  
  if (!is.null(ReferencePoint)) {
    # Set individual to TRUE if a refernce point is given
    Individual <- TRUE
    if (IsReferenceType[ReferencePoint]) {
      # Remember the name of the reference point in the dataset of reference type
      ReferencePoint_name <- row.names(X$marks[ReferencePoint, ])
    } else {
      # The reference point must be in the reference type
      stop("The reference point must be of the reference point type.")
    }
  }
  
  # Global ratio
  if (ReferenceType==NeighborType | CaseControl) {
    WrMinusReferencePoint <- sum(X$marks$PointWeight[IsReferenceType])-X$marks$PointWeight
    Wn <- WrMinusReferencePoint[IsReferenceType]
  } else {
    Wn <- sum(X$marks$PointWeight[IsNeighborType])
  }
  if (CaseControl) {
    Wa <- sum(X$marks$PointWeight[IsNeighborType]) 
  } else {
    WaMinusReferencePoint <- sum(X$marks$PointWeight)-X$marks$PointWeight
    Wa <- WaMinusReferencePoint[IsReferenceType]
  }
  GlobalRatio <- Wn/Wa
  
  Nr <- length(r)
  # Neighborhoods (i.e. all neighbors of a point less than a distance apart)
  # Store weights of neighbors of interest in first Nr columns, all points from Nr+1 to 2*Nr

  # Call C routine to fill Nbd
  if (CaseControl) {
    if (inherits(X, "Dtable")) {
      # Dtable case
      Nbd <- parallelCountNbdDtCC(r, X$Dmatrix, X$marks$PointWeight, IsReferenceType, IsNeighborType)
    } else {
      # wmppp case
      Nbd <- parallelCountNbdCC(r, X$x, X$y, X$marks$PointWeight, IsReferenceType, IsNeighborType)
    }
  } else {
    if (inherits(X, "Dtable")) {
      # Dtable case
      Nbd <- parallelCountNbdDt(r, X$Dmatrix, X$marks$PointWeight, IsReferenceType, IsNeighborType)
    } else {
      # wmppp case
      Nbd <- parallelCountNbd(r, X$x, X$y, X$marks$PointWeight, IsReferenceType, IsNeighborType)
    }
  }
  
  # Cumulate weights up to each distance
  NbdInt <- t(apply(Nbd[, seq_len(Nr)], 1, cumsum))
  NbdAll <- t(apply(Nbd[, (Nr + 1):(2 * Nr)], 1, cumsum))
  
  # Calculate the ratio of points of interest around each point
  LocalRatio <- NbdInt/NbdAll
  
  if (is.null(ReferencePoint)) {
    # Divide it by the global ratio. Ignore points with no neighbor at all.
    Mvalues <- apply(LocalRatio, 2, function(x) sum(x[is.finite(x)])/sum(GlobalRatio[is.finite(x)]))
    # Keep individual values
    if (Individual) {
      Mvalues <- cbind(Mvalues, t(LocalRatio/GlobalRatio))
    }
  } else {
    # Find the reference point in the set of points of the reference type
    ReferencePoint_index <- which(rownames(X[IsReferenceType]$marks) == ReferencePoint_name)
    # Only keep the value of the reference point
    Mvalues <- LocalRatio[ReferencePoint_index, ]/GlobalRatio[ReferencePoint_index]
  }
  
  # Put the results into an fv object
  MEstimate <- data.frame(r, rep(1, length(r)), Mvalues)
  ColNames <- c("r", "theo", "M")
  Labl <- c("r", "%s[theo](r)", "hat(%s)(r)")
  Desc <- c("Distance argument r", "Theoretical independent %s", "Estimated %s")  
  if (Individual & is.null(ReferencePoint)) {
    # ColNumbers will usually be line numbers of the marks df, but may be real names.
    ColNumbers <- row.names(X$marks[IsReferenceType, ])
    ColNames <- c(ColNames, paste("M", ColNumbers, sep="_"))
    Labl <- c(Labl, paste("hat(%s)[", ColNumbers, "](r)", sep=""))
    Desc <- c(Desc, paste("Individual %s around point", ColNumbers))
  }
  colnames(MEstimate) <- ColNames
  
  # Make an fv object
  M <- fv(MEstimate, argu="r", ylab=quote(M(r)), valu="M", 
          fmla= "cbind(M,theo)~r", alim=c(0, max(r)), labl=Labl, 
          desc=Desc, unitname=X$window$unit, fname="M")
  fvnames(M, ".") <- ColNames[-1]
  
  
  # Calculate the quantiles of the individual values with respect to the null hypothesis
  if (Quantiles) {
    # Run Monte Carlo simulations of Mhat for each reference point
    nReferencePoints <- sum(IsReferenceType)
    # Prepare a matrix to save quantiles
    MQuantiles <- matrix(0, nrow = length(r), ncol = nReferencePoints)
    colnames(MQuantiles) <- paste("M", ColNumbers, sep="_")
    rownames(MQuantiles) <- r
    if (verbose) ProgressBar <- utils::txtProgressBar(min = 0, max = nReferencePoints)
    for (i in seq_len(nReferencePoints)) {
      # Null hypothesis
      SimulatedPP <- expression(
        rRandomLocation(
          X, 
          ReferencePoint = which(X$marks$PointType == ReferenceType)[i], 
          CheckArguments = FALSE
        )
      )
      # Compute the simulations. The envelope is useless: we need the simulated values.
      Envelope <- envelope(
        # The value Mhat(X) is not used but the point #1 must be of ReferenceType
        # so retain points of ReferenceType only to save time
        X[X$marks$PointType == ReferenceType], 
        fun = Mhat, 
        nsim = NumberOfSimulations, 
        # nrank may be any value because the envelope is not used
        nrank = 1,
        # Arguments for Mhat()
        r = r, 
        ReferenceType = ReferenceType, 
        NeighborType = NeighborType, 
        CaseControl = CaseControl, 
        Individual = TRUE, 
        # The reference point is always 1 after rRandomLocation()
        ReferencePoint = 1, 
        CheckArguments = FALSE,
        # Arguments for envelope()
        simulate = SimulatedPP, 
        # Do not show the progress
        verbose = FALSE, 
        # Save individual simulations into attribute simfuns
        savefuns = TRUE
      )
      # Get the distribution of simulated values (eliminate the "r" column). 
      Simulations <- as.matrix(attr(Envelope, "simfuns"))[, -1]
      # Calculate the quantiles of observed values
      for (r_i in seq_along(r)) {
        if (any(!is.na(Simulations[r_i, ]))) {
          # Check that at least one simulated value is not NaN so that ecdf() works.
          MQuantiles[r_i, i] <- stats::ecdf(Simulations[r_i, ])(Mvalues[r_i, i])
        } else {
          MQuantiles[r_i, i] <- NaN
        }
      }
      # Progress bar
      if (verbose) utils::setTxtProgressBar(ProgressBar, i)
    }
    if (verbose) close(ProgressBar)
    # Save the quantiles as an attribute of the fv
    attr(M, "Quantiles") <- MQuantiles
    attr(M, "Alpha") <- Alpha
  }
  
  if (Individual & is.null(ReferencePoint)) {
    # Save the reference type for future smoothing
    attr(M, "ReferenceType") <- ReferenceType
  }
  return(M)
}
