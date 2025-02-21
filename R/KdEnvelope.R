KdEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType,
    NeighborType = ReferenceType,
    Weighted = FALSE,
    Original = TRUE,
    Approximate = ifelse(X$n < 10000, 0, 1),
    Adjust = 1,
    MaxRange = "ThirdW",
    StartFromMinR = FALSE,
    SimulationType = "RandomLocation",
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE,
    parallel_pgb_refresh = 1/10) {

  CheckdbmssArguments()

  if (parallel && methods::is(future::plan(), "sequential")) WarnPlan()

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
    stop(paste("The null hypothesis", sQuote(SimulationType), "has not been recognized."))
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
  # local envelope, keep extreme values for lo and hi (nrank=1)
  Envelope <- envelope(
    X,
    fun = Kdhat,
    nsim = nSimSerial,
    nrank = 1,
    r = r,
    ReferenceType = ReferenceType,
    NeighborType = NeighborType,
    Weighted = Weighted,
    Original = Original,
    Approximate = Approximate,
    Adjust = Adjust,
    MaxRange = MaxRange,
    StartFromMinR = StartFromMinR,
    CheckArguments = FALSE,
    simulate = SimulatedPP,
    verbose = (verbose & nSimParallel == 0),
    savefuns = TRUE
  )

  # Parallel
  if (nSimParallel > 0) {
    # Run simulations
    progress <- progressr::progressor(
      steps = round(nSimParallel * parallel_pgb_refresh)
    )
    # Calculated only once for performance
    parallel_pgb_refresh_inverse <- 1 / parallel_pgb_refresh
    # Declare the iterator to avoid R CMD check  note
    Simulation <- 0
    # Simulation loop
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
      # The value of Kd is in $Kd
      Kd <- Kdhat(
        SimulatedPP,
        r = r,
        ReferenceType = ReferenceType,
        NeighborType = NeighborType,
        Weighted = Weighted,
        Original = Original,
        Approximate = Approximate,
        Adjust = Adjust,
        MaxRange = MaxRange,
        StartFromMinR = StartFromMinR,
        CheckArguments = FALSE
      )$Kd
      # Progress every nSimParallel * parallel_pgb_refresh steps
      if (Simulation %% parallel_pgb_refresh_inverse == 0) progress()
      Kd
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
