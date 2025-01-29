Dhat <- function(
    X,
    r = NULL,
    Cases,
    Controls = NULL,
    Intertype = FALSE,
    CheckArguments = TRUE) {

  if (CheckArguments) {
    CheckdbmssArguments()
  }
  # K of cases.
  KCases <- Khat(
    X,
    r = r,
    ReferenceType = Cases,
    NeighborType = Cases,
    CheckArguments = FALSE
  )
  # Default controls are all points except cases. Reserved name is "CoNtRoLs_"
  Y <- X
  if (is.null(Controls)) {
    Controls <- "CoNtRoLs_"
    if (Controls %in% levels(marks(Y)$PointType)) {
      stop("A point type is named 'CoNtRoLs_'. It must be changed to use the 'Controls = NULL' option of Dhat.")
    }
    levels(marks(Y)$PointType) <- c(levels(marks(Y)$PointType), Controls)
    marks(Y)$PointType[marks(Y)$PointType != Cases] <- Controls
  }
  # K of controls. r must be those of cases.
  if (Intertype) {
    KControls <- Khat(
      Y,
      r = KCases$r,
      ReferenceType = Cases,
      NeighborType = Controls,
      CheckArguments = FALSE
    )
  } else {
    KControls <- Khat(
      Y,
      r = KCases$r,
      ReferenceType = Controls,
      NeighborType = Controls,
      CheckArguments = FALSE
    )
  }
  # Calculate the difference (a difference between fv's yields a dataframe)
  Dvalues <- KCases - KControls
  DEstimate <- cbind(
    as.data.frame(KCases)[1],
    as.data.frame(Dvalues)[2:3]
  )

  # Return the values of D(r)
  D <- fv(
    DEstimate,
    argu = "r",
    ylab = quote(D(r)),
    valu = attr(KCases, "valu"),
    fmla = attr(KCases, "fmla"),
    alim = attr(KCases, "alim"),
    labl = c("r", "%s[theo](r)", "hat(%s)[iso](r)"),
    desc = attr(KCases, "desc"),
    unitname = attr(KCases, "unitname"),
    fname = "D"
  )
  fvnames(D, ".") <- colnames(DEstimate)[-1]
  return(D)
}
