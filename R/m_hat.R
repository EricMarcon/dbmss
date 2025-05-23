mhat <- function(
    X,
    r = NULL,
    ReferenceType,
    NeighborType = ReferenceType,
    CaseControl = FALSE,
    Original = TRUE,
    Approximate = ifelse(X$n < 10000, 0, 1),
    Adjust = 1,
    MaxRange = "ThirdW",
    Individual = FALSE,
    CheckArguments = TRUE) {

  # Eliminate erroneous configurations
  if (CheckArguments) {
    CheckdbmssArguments()
    if (CaseControl & (ReferenceType == NeighborType)) {
      warning("Cases and controls are identical.")
      return(rep(1, length(r)))
    }
  }

  # Vectors to recognize point types
  IsReferenceType <- (marks(X)$PointType == ReferenceType)
  IsNeighborType <- (marks(X)$PointType == NeighborType)

  # Global ratio
  if (ReferenceType == NeighborType | CaseControl) {
    WrMinusReferencePoint <-
      sum(marks(X)$PointWeight[IsReferenceType]) - marks(X)$PointWeight
    Wn <- WrMinusReferencePoint[IsReferenceType]
  } else {
    Wn <- sum(marks(X)$PointWeight[IsNeighborType])
  }
  if (CaseControl) {
    Wa <- sum(marks(X)$PointWeight[IsNeighborType])
  } else {
    WaMinusReferencePoint <- sum(marks(X)$PointWeight) - marks(X)$PointWeight
    Wa <- WaMinusReferencePoint[IsReferenceType]
  }
  GlobalRatio <- Wn / Wa

  # Roughly estimate max distances
  if (is.null(r)) {
    rmin <- Diameter <- 0
    # Estimate max distance in the point set
    if (inherits(X, "Dtable")) {
      # Dtable case
      Diameter <- max(X$Dmatrix)
    } else {
      # wmppp case, approximate rmax by the diameter of the window
      Diameter <- diameter(X$win)
    }
    # Set rmax to window /2, /3 or /4. DO2005 is ignored at this stage.
    rmax <- switch(
      MaxRange,
      HalfW = Diameter / 2,
      ThirdW =  Diameter / 3,
      QuarterW = Diameter / 4
    )
    if (is.null(rmax)) {
      rmax <- Diameter / 3
    }
  } else {
    rmin <- 0
    rmax <- max(r)
  }

  if (Approximate & !inherits(X, "Dtable")) {
    # Round distances to save memory
    # Prepare steps so that 1024*Approximate steps are between 0 and rmax.
    # Pairs further than 2*rmax apart are dropped.
    rseq <- seq(from = rmin, to = rmax*2, length.out = 2048 * Approximate)
    Nr <- length(rseq)

    # Call C routine to fill Nbd (1 line per reference point, 2*Nr columns)
    if (CaseControl) {
      Nbd <- parallelCountNbdCC(
        rseq,
        X$x,
        X$y,
        marks(X)$PointWeight,
        IsReferenceType,
        IsNeighborType
      )
    } else {
      Nbd <- parallelCountNbd(
        rseq,
        X$x,
        X$y,
        marks(X)$PointWeight,
        IsReferenceType,
        IsNeighborType
      )
    }

    # Adjust distances: values are the centers of intervals
    rseq <- c(0, (rseq[2:Nr] + rseq[seq_len(Nr) - 1]) / 2)

    # Estimate the bandwidth according to Adjust if requested.
    # Distances are the values of rseq corresponding to at least
    # a pair of points (reference and neighbor)
    if (Original) {
      h <- stats::bw.nrd0(rseq[colSums(Nbd[, seq_len(Nr)]) > 0]) * Adjust
    } else {
      h <- stats::bw.SJ(rseq[colSums(Nbd[, seq_len(Nr)]) > 0]) * Adjust
    }

    # Calculate densities of neighbors
    # (with unnormalized weights so suppress warnings)
    Djc <- t(
      apply(
        Nbd[, seq_len(Nr)],
        MARGIN = 1,
        FUN = function(x) {
          suppressWarnings(
            stats::density(
              rseq,
              bw = h,
              weights = x,
              from = rmin,
              to = rmax,
              na.rm = TRUE
            )
          )$y
        }
      )
    )
    Dj <- t(
      apply(
        Nbd[, (Nr + 1):(2 * Nr)],
        MARGIN = 1,
        FUN = function(x) {
          suppressWarnings(
            stats::density(
              rseq,
              bw = h,
              weights = x,
              from = rmin,
              to = rmax,
              na.rm = TRUE
            )
          )$y
        }
      )
    )
    # Get the x values of the density estimation: estimate one vector
    x <- stats::density(
      rseq,
      bw = h,
      from = rmin,
      to = rmax,
      na.rm = TRUE
    )$x

  } else {
    # Classical estimation

    if (inherits(X, "Dtable")) {
      # Dtable case: set distances between identical points to NA and keep reference points only
      Nbd <- X$Dmatrix
      diag(Nbd) <- NA
      # Nbd already contains distances between points
      Nbd <- Nbd[IsReferenceType, ]
    } else {
      # wmppp case:
      # Call C routine to fill Nbd: a distance matrix, reference points in lines, neighbors in columns
      # Send x, y, and the list of reference points (indexed from 0 in C instead of 1 in R)
      Nbd <- parallelCountNbdm(X$x, X$y, which(IsReferenceType) - 1)
      # Negative values are actually NA
      Nbd[Nbd < 0] <- NA
    }

    # Choose the bandwidth based on all distance pairs between reference and neighbor points
    # Prepare the data
    RefDistances <- Nbd[, IsNeighborType]
    if (ReferenceType == NeighborType) {
      # RefDistances is a square matrix: keep the upper half as a vector
      RefDistances <- RefDistances[upper.tri(RefDistances)]
    }
    # Only pairs of points up to 2 rmax are considered for consistency
    # with approximated computation
    if (Original) {
      h <- stats::bw.nrd0(RefDistances[RefDistances <= rmax * 2]) * Adjust
    } else {
      h <- stats::bw.SJ(RefDistances[RefDistances <= rmax * 2]) * Adjust
    }

    if (is.null(r)) {
      # Min distance obtained from the data rather than 0
      rmin <- min(Nbd, na.rm = TRUE)
      # Max distance may be obtained from the data rather than from the window
      if (MaxRange == "DO2005") {
        rmax <- stats::median(Nbd, na.rm = TRUE)
      }
    }

    # Calculate densities of neighbors (with unnormalized weights =>
    # 'subdensity=TRUE' / suppress warnings in R <= 4.2.0)
    # R 4.2 fixes bug with missing x and weights: this code is from Martin Maechler
    wdensityNA <-
      if (getRversion() >= "4.2.0") {
        # fixed bug PR#18151 (Aug.2021)
        function(x, weights) {
          stats::density(
            x,
            bw = h,
            weights = weights,
            subdensity = TRUE,
            from = rmin,
            to = rmax,
            na.rm = TRUE
          )$y
        }
      } else {
        function(x, weights) {
          suppressWarnings(
            stats::density(
              x,
              bw = h,
              weights = weights[!is.na(x)],
              from = rmin,
              to = rmax,
              na.rm = TRUE)
          )$y
        }
      }
    Djc <- t(
      apply(
        Nbd[, IsNeighborType],
        MARGIN = 1,
        FUN = function(x) {
          wdensityNA(x, weights = marks(X)$PointWeight[IsNeighborType])
        }
      )
    )
    Dj <- t(
      apply(
        Nbd,
        MARGIN = 1,
        function(x) {
          wdensityNA(x, weights = marks(X)$PointWeight)
        }
      )
    )

    # Get the x values of the density estimation: estimate one vector
    x <- stats::density(
      Nbd[1, IsNeighborType],
      bw = h,
      from = rmin,
      to = rmax,
      na.rm = TRUE
    )$x
  }


  # Calculate the local ratio (at distance r)
  LocalRatio <- Djc / Dj
  # Divide it by the global ratio. Ignore points with no neighbor at all.
  mvalues <- matrix(colSums(LocalRatio) / sum(GlobalRatio))
  # Keep individual values
  if (Individual) {
    mvalues <- cbind(mvalues, t(LocalRatio / GlobalRatio))
  }

  # Interpolate if necessary
  if (is.null(r)) {
    r <- x
  } else {
    mvalues <- apply(
      mvalues,
      MARGIN = 2,
      function(m) stats::approx(x, y = m, xout = r)$y
    )
  }
  # Put the results into an fv object
  mEstimate <- data.frame(r, rep(1, length(r)), mvalues)
  ColNames <- c("r", "theo", "m")
  Labl <- c("r", "%s[theo](r)", "hat(%s)(r)")
  Desc <- c("Distance argument r", "Theoretical independent %s", "Estimated %s")
  if (Individual) {
    # ColNumbers will usually be line numbers of the marks df, but may be real names.
    ColNumbers <- row.names(marks(X)[IsReferenceType, ])
    ColNames <- c(ColNames, paste("m", ColNumbers, sep = "_"))
    Labl <- c(Labl, paste("hat(%s)[", ColNumbers, "](r)", sep = ""))
    Desc <- c(Desc, paste("Individual %s around point", ColNumbers))
  }
  colnames(mEstimate) <- ColNames

  # Make an fv object
  m <- fv(
    mEstimate,
    argu = "r",
    ylab = quote(m(r)),
    valu = "m",
    fmla = "cbind(m,theo)~r",
    alim = c(0, max(r)),
    labl = Labl,
    desc = Desc,
    unitname = X$window$unit,
    fname = "m"
  )
  fvnames(m, ".") <- ColNames[-1]
  if (Individual) {
    # Save the reference type for future smoothing
    attr(m, "ReferenceType") <- ReferenceType
  }
  # Return the values of m(r)
  return(m)
}
