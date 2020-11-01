Mhat <-
function(X, r = NULL, ReferenceType, NeighborType = ReferenceType, 
         CaseControl = FALSE, Individual = FALSE, CheckArguments = TRUE)
{
  # Eliminate erroneous configurations
  if (CheckArguments) {
    CheckdbmssArguments()
    if (CaseControl & (ReferenceType==NeighborType)) {
      warning("Cases and controls are identical.")
      return(rep(1,length(r)))
    }
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
  NbdInt <- t(apply(Nbd[, 1:Nr], 1, cumsum))
  NbdAll <- t(apply(Nbd[, (Nr+1):(2*Nr)], 1, cumsum))
  
  # Calulate the ratio of points of interest around each point
  LocalRatio <- NbdInt/NbdAll
  # Divide it by the global ratio. Ignore points with no neighbor at all.
  Mvalues <- apply(LocalRatio, 2, function(x) sum(x[is.finite(x)])/sum(GlobalRatio[is.finite(x)]))
  # Keep individual values
  if (Individual) {
    Mvalues <- cbind(Mvalues, t(LocalRatio/GlobalRatio))
  }
  
  # Put the results into an fv object
  MEstimate <- data.frame(r, rep(1, length(r)), Mvalues)
  ColNames <- c("r", "theo", "M")
  Labl <- c("r", "%s[ind](r)", "hat(%s)(r)")
  Desc <- c("Distance argument r", "Theoretical independent %s", "Estimated %s")  
  if (Individual) {
    # ColNumbers will usually be line numbers of the marks df, but may be real names.
    ColNumbers <- row.names(X$marks[IsReferenceType, ])
    ColNames <- c(ColNames, paste("M", ColNumbers, sep="_"))
    Labl <- c(Labl, paste("hat(%s)[", ColNumbers, "](r)", sep=""))
    Desc <- c(Desc, paste("Individual %s around point", ColNumbers))
  }
  colnames(MEstimate) <- ColNames
  
  # Return the values of M(r)
  return (fv(MEstimate, argu="r", ylab=quote(M(r)), valu="M", fmla= "cbind(M,theo)~r", alim=c(0, max(r)), labl=Labl, desc=Desc, unitname=X$window$unit, fname="M"))
}
