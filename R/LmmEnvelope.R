LmmEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType = "",
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE,
    parallel_pgb_refresh = 1/10) {

  # Calculate the envelope of Kmm
  Envelope <- KmmEnvelope(
    X,
    r = r,
    NumberOfSimulations = NumberOfSimulations,
    Alpha = Alpha,
    ReferenceType = ReferenceType,
    Global = Global,
    verbose = verbose,
    parallel = parallel,
    parallel_pgb_refresh = parallel_pgb_refresh
  )
  # Transform K to L
  Columns <- names(Envelope)[-1]
  for (i in Columns) {
    Envelope[[i]] <- sqrt(Envelope[[i]] / pi) - Envelope$r
  }
  attr(Envelope, "ylab") <- "Lmm(r)"
  attr(Envelope, "yexp") <- "Lmm(r)"
  attr(Envelope, "fname") <- "Lmm"
  return(Envelope)
}
