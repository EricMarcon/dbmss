LEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType = "",
    NeighborType = "",
    SimulationType = "RandomPosition",
    Precision = 0,
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE,
    parallel_pgb_refresh = 1/10) {

  # Calculate the envelope of K
  Envelope <- KEnvelope(
    X,
    r = r,
    NumberOfSimulations = NumberOfSimulations,
    Alpha = Alpha,
    ReferenceType = ReferenceType,
    NeighborType = NeighborType,
    SimulationType = SimulationType,
    Precision = Precision,
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
  attr(Envelope, "ylab") <- attr(Envelope, "yexp") <- quote(L(r))
  attr(Envelope, "fname") <- "L"
  return(Envelope)
}
