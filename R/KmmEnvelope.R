KmmEnvelope <- function(
    X,
    r = NULL,
    NumberOfSimulations = 100,
    Alpha = 0.05,
    ReferenceType = "",
    Global = FALSE,
    verbose = interactive(),
    parallel = FALSE,
    parallel_pgb_refresh = 1/10) {

  CheckdbmssArguments()

  if (parallel && methods::is(future::plan(), "sequential")) WarnPlan()

  # The only null hypothesis is random labeling (equivalently, random location)
  SimulatedPP <- expression(
    rRandomLocation(X, ReferenceType = ReferenceType, CheckArguments = FALSE)
  )

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
    fun = Kmmhat,
    nsim = nSimSerial,
    nrank = 1,
    r = r,
    ReferenceType = ReferenceType,
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
      SimulatedPP <- rRandomLocation(
        X,
        ReferenceType = ReferenceType,
        CheckArguments = FALSE
      )
      # The value of K is the the third column of the fv, i.e. third item
      K <- Kmmhat(
        SimulatedPP,
        r = r,
        ReferenceType = ReferenceType,
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

  attr(Envelope, "einfo")$H0 <- "Random Location"

  # Calculate confidence intervals
  Envelope <- FillEnvelope(Envelope, Alpha = Alpha, Global = Global)
  # Return the envelope
  return(Envelope)
}
