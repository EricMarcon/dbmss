GoFtest <- function(Envelope) {

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("Envelope is not of class envelope")
  }
  # Verify simulations
  if (is.null(attr(Envelope, "simfuns"))) {
    stop("Envelope does not contain simulations in its attribute simfuns")
  } else {
    r <- as.data.frame(attr(Envelope, "simfuns"))[, 1]
    ActualValues <- Envelope$obs
    SimulatedValues <- as.data.frame(attr(Envelope, "simfuns"))[, -1]
  }

  NumberOfSimulations <- dim(SimulatedValues)[2]
  AverageSimulatedValues <- apply(SimulatedValues, 1, sum) / (NumberOfSimulations - 1)
  rIncrements <- (r - c(0,r)[seq_along(r)])[-1]

  # Ui calculate the statistic for a simulation
  Ui <- function(SimulationNumber) {
    Departure <- (SimulatedValues[, SimulationNumber] -
      AverageSimulatedValues)[seq_along(r) - 1]
    WeightedDeparture <- (Departure[!is.nan(Departure)])^2 *
      rIncrements[!is.nan(Departure)]
    return(sum(WeightedDeparture))
  }

  # Calculate the Ui statistic for all simulations
  SimulatedU <- vapply(
    seq_len(NumberOfSimulations),
    FUN = Ui,
    FUN.VALUE = 0
  )

  # Calculate the statistic for the actual value
  RecenteredValues <- (ActualValues - AverageSimulatedValues)[seq_along(r) - 1]
  WeightedRecenteredValues <- (RecenteredValues[!is.nan(RecenteredValues)])^2 *
    rIncrements[!is.nan(RecenteredValues)]
  ActualU <- sum(WeightedRecenteredValues)

  # Return the rank
  return(mean(ActualU < SimulatedU))
}


GoFtest <- function(
    Envelope,
    Scaling = "Asymmetric",
    Distance = "Integral2",
    Range = NULL) {

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("Envelope is not of class envelope")
  }
  # Verify Scaling
  if (!is.character(Scaling) | !is.vector(Scaling) | !length(Scaling) == 1) {
    stop("Argument 'Scaling' must be a character vector of length one")
  }
  if (!(Scaling %in% c("Quantile", "Studentized", "Asymmetric", "None"))) {
    stop("Invalid argument: 'Scaling'. Accepted arguments are: Quantile,
    Studentized, Asymmetric, None.")
  }
  # Verify Distance
  if (!is.character(Distance) | !is.vector(Distance) | !length(Distance) == 1) {
    stop("Argument 'Distance' must be a character vector of length one")
  }
  if (!(Distance %in% c("Integral2", "Integral", "Maximum"))) {
    stop("Invalid argument: 'Distance'. Accepted arguments are: Integral2, Integral,
         Maximum.")
  }
  # Verify Range
  if (!is.null(Range) && (!is.vector(Range) |
                          !is.numeric(Range) |
                          length(Range) != 2)) {
    stop("Invalid argument: 'Range'. Accepted arguments are a numeric vector of length two,
         specifying the minimum and maximum distances over which to compute the test.")
  }
  # Verify simulations
  if (is.null(attr(Envelope, "simfuns"))) {
    stop("Envelope does not contain simulations in its attribute simfuns")
  } else {
    r <- as.data.frame(attr(Envelope, "simfuns"))[, 1]
    ActualValues <- Envelope$obs
    SimulatedValues <- as.data.frame(attr(Envelope, "simfuns"))[, -1]
  }

  # Transform observed Ls into K for the test (L envelopes are constructed from
  # the K function)
  if (attr(Envelope, "fname") %in% c("L", "Lmm")) {
    ActualValues <- (ActualValues + r)^2 * pi
  }

  # Restrict analysis to chosen distance range
  if (!is.null(Range)) {
    if(min(Range) < max(r) && max(Range) > min(r)) {
      SelectedR <- (r > min(Range) & r < max(Range))
      ActualValues <- ActualValues[SelectedR]
      SimulatedValues <- SimulatedValues[SelectedR, ]
      r <- r[SelectedR]
    } else {
      warning("The selected range is outside the simulated distances.
              The test was computed using all distances from the envelope.")
    }
  }

  NumberOfSimulations <- dim(SimulatedValues)[2]
  AverageSimulatedValues <- apply(SimulatedValues, 1, sum) / (NumberOfSimulations - 1)
  rIncrements <- (r - c(0, r)[seq_along(r)])[-1]

  # Calculate the weights to scale the residuals of the statistic
  Weights <- switch(Scaling,
                    "Studentized" = 1/apply(SimulatedValues, 1, sd, na.rm = T),
                    "Quantile" = 1/(apply(SimulatedValues, 1,
                                          quantile, probs = 0.975, na.rm = T) -
                                      apply(SimulatedValues, 1,
                                            quantile, probs = 0.025, na.rm = T)),
                    "Asymmetric" = {
                      Upper <- 1/(apply(SimulatedValues, 1,
                                        quantile, probs = 0.975,na.rm = T) -
                                    AverageSimulatedValues)
                      Lower <- 1/(AverageSimulatedValues -
                                    apply(SimulatedValues, 1,
                                          quantile, probs = 0.025, na.rm = T))
                      list(UprW = Upper, LwrW = Lower)
                    },
                    "None" = rep(1, length(r)))

  # Ui calculate the statistic for a simulation
  Ui <- function(SimulationNumber, ValueToTest) {
    Departure <- (ValueToTest[, SimulationNumber] -
                    AverageSimulatedValues)[seq_along(r) - 1]
    if (inherits(Weights, "list")) {
      ScaledDeparture <- sapply(seq_along(Departure),
                                FUN= function(x) ifelse(Departure[x]>=0,
                                                        Departure[x]*Weights$UprW[x],
                                                        Departure[x]*Weights$LwrW[x]))
      ScaledDeparture <- as.vector(ScaledDeparture)
    } else {
      ScaledDeparture <- Departure*Weights[seq_along(r) - 1]
    }
    GofStatistic <- switch(Distance,
                           "Integral2" =
                             sum((ScaledDeparture[!is.nan(ScaledDeparture)])^2 *
                                   rIncrements[!is.nan(ScaledDeparture)],
                                 na.rm = T),
                           "Integral" =
                             sum(abs((ScaledDeparture[!is.nan(ScaledDeparture)])) *
                                   rIncrements[!is.nan(ScaledDeparture)],
                                 na.rm = T),
                           "Maximum" = max(ScaledDeparture, na.rm = T))
    return(GofStatistic)
  }

  # Calculate the Ui statistic for all simulations
  SimulatedU <- vapply(
    seq_len(NumberOfSimulations),
    FUN = Ui,
    FUN.VALUE = 0,
    ValueToTest = SimulatedValues
  )

  # Calculate the Ui statistic for the actual value
  ActualU <- vapply(
    1,
    FUN = Ui,
    FUN.VALUE = 0,
    ValueToTest = data.frame(ActualValues)
  )

  # Return the rank
  return(mean(ActualU < SimulatedU))
}
