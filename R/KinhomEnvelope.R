KinhomEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType = "",
    lambda = NULL,
    SimulationType = "RandomPosition",
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE,
    parallel_pgb_refresh = 1/10) {

  CheckdbmssArguments()

  if (parallel && methods::is(future::plan(), "sequential")) WarnPlan()

  # Estimate intensity if it has not been provided.
  if (is.null(lambda)) {
    if (ReferenceType == "") {
      X.reduced <- X
    } else {
      X.reduced <- X[marks(X)$PointType == ReferenceType]
    }
    lambda <- spatstat.explore::density.ppp(
      X.reduced,
      sigma = bw.diggle(X.reduced)
    )
  }

  # Choose the null hypothesis
  SimulatedPP <- switch(
    SimulationType,
    RandomPosition = expression(spatstat.random::rpoispp(lambda)),
    RandomLocation = expression(rRandomLocation(X, CheckArguments = FALSE)),
    RandomLabeling = expression(rRandomLabeling(X, CheckArguments = FALSE)),
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
  # local envelope, keep extreme values for lo and hi (nrank=1)
  Envelope <- suppressWarnings(
    # Suppress warning:
    # Envelope may be invalid; argument ‘lambda’ appears to have been fixed.
    # because lambda is necessarily fixed here.
    envelope(
      X,
      fun = Kinhomhat,
      nsim = nSimSerial,
      nrank = 1,
      r = r,
      ReferenceType = ReferenceType,
      lambda = lambda,
      CheckArguments = FALSE,
      simulate = SimulatedPP,
      verbose = (verbose & nSimParallel == 0),
      savefuns = TRUE
    )
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
        RandomPosition = spatstat.random::rpoispp(lambda),
        RandomLocation = rRandomLocation(X, CheckArguments = FALSE),
        RandomLabeling = rRandomLabeling(X, CheckArguments = FALSE),
        PopulationIndependence = rPopulationIndependenceM(
          X,
          ReferenceType = ReferenceType,
          CheckArguments = FALSE
        )
      )
      # The value of K is the the third column of the fv, i.e. third item
      K <- Kinhomhat(
        SimulatedPP,
        r = r,
        ReferenceType = ReferenceType,
        lambda = lambda,
        CheckArguments = FALSE
      )[[3]]
      # Progress every nSimParallel * parallel_pgb_refresh steps
      if (Simulation %% parallel_pgb_refresh_inverse == 0) progress()
      K
    }
    # Merge the values into the envelope
    attr(Envelope, "simfuns") <- cbind(attr(Envelope, "simfuns"), ParalellSims)
    attr(Envelope, "einfo")$nsim <- NumberOfSimulations
    attr(Envelope, "einfo")$Nsim <- NumberOfSimulations
  }

  attr(Envelope, "einfo")$H0 <- switch(
    SimulationType,
    RandomPosition = "Random Position",
    RandomLocation = "Random Location",
    RandomLabeling = "Random Labeling",
    PopulationIndependence = "Population Independence"
  )
  # Calculate confidence intervals
  Envelope <- FillEnvelope(Envelope, Alpha = Alpha, Global = Global)
  # Return the envelope
  return(Envelope)
}
