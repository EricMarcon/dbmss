MEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType,
    NeighborType = ReferenceType,
    CaseControl = FALSE,
    SimulationType = "RandomLocation",
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE) {

  CheckdbmssArguments()

  if (parallel && methods::is(future::plan(), "sequential")) {
    warning(
      c(
        "You chose parallel computing but the strategy is sequential.\n",
        "You may want to set a plan such as\n
        `library(future)`
        `plan(multisession, workers = availableCores(omit = 1))`"
      )
    )
  }

  # Choose the null hypothesis
  SimulatedPP <- switch(
    SimulationType,
    RandomLocation = expression(rRandomLocation(X, CheckArguments = FALSE)),
    RandomLabeling = expression(rRandomLabelingM(X, CheckArguments = FALSE)),
    PopulationIndependence = expression(
      rPopulationIndependenceM(
        X,
        ReferenceType = ReferenceType,
        CheckArguments = FALSE
      )
    )
  )
  if (is.null(SimulatedPP)) {
    stop(
      paste(
        "The null hypothesis",
        sQuote(SimulationType),
        "has not been recognized."
      )
    )
  }

  # Parallel?
  if (parallel & NumberOfSimulations > 4) {
    nSimSerial <- 4
    nSimParallel <-  NumberOfSimulations - 4
  } else {
    nSimSerial <- NumberOfSimulations
    nSimParallel <- 0
  }

  # Serial.
  # local envelope, keep extreme values for lo and hi (nrank = 1)
  Envelope <- envelope(
    X,
    fun = Mhat,
    nsim = nSimSerial,
    nrank = 1,
    r = r,
    ReferenceType = ReferenceType,
    NeighborType = NeighborType,
    CaseControl = CaseControl,
    CheckArguments = FALSE,
    simulate = SimulatedPP,
    verbose = ifelse(nSimParallel > 0, FALSE, verbose),
    savefuns = TRUE
  )

  # Parallel
  if (nSimParallel > 0) {
    # Run simulations
    progress <- progressr::progressor(steps = nSimParallel / 100)
    ParalellSims <- foreach::foreach(
      Simulation = seq_len(nSimParallel),
      .combine = cbind,
      .options.future = list(seed = TRUE)
    ) %dofuture% {
      SimulatedPP <- switch(
        SimulationType,
        RandomLocation = rRandomLocation(X, CheckArguments = FALSE),
        RandomLabeling = rRandomLabelingM(X, CheckArguments = FALSE),
        PopulationIndependence = rPopulationIndependenceM(
          X,
          ReferenceType = ReferenceType,
          CheckArguments = FALSE
        )
      )
      M <- Mhat(
        SimulatedPP,
        r = r,
        ReferenceType = ReferenceType,
        NeighborType = NeighborType,
        CaseControl = CaseControl,
        CheckArguments = FALSE
      )$M
      # Progress every percent
      if (Simulation %% 100 == 0) progress()
      M
    }
    # Merge the values into the envelope
    attr(Envelope, "simfuns") <- cbind(attr(Envelope, "simfuns"), ParalellSims)
    attr(Envelope, "einfo")$nsim <- NumberOfSimulations
    attr(Envelope, "einfo")$Nsim <- NumberOfSimulations
  }

  attr(Envelope, "einfo")$H0 <- switch(
    SimulationType,
    RandomLocation = "Random Location",
    RandomLabeling = "Random Labeling",
    PopulationIndependence = "Population Independence"
  )
  # Calculate confidence intervals
  Envelope <- FillEnvelope(Envelope, Alpha = Alpha, Global = Global)
  # No edge effect correction
  attr(Envelope, "einfo")$valname <- NULL
  # Return the envelope
  return(Envelope)
}
