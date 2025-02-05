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
    Scaling = "asymmetric",
    Method = "Integral") {

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("Envelope is not of class envelope")
  }
  # Verify Scaling
  if(!is.character(Scaling) && !is.vector(Scaling) && !length(Scaling) == 1) {
    stop("Argument 'Scaling' must be a character vector of length one")
  }
  if(!(Scaling %in% c("quantile", "studentized", "asymmetric", "none"))) {
    stop("Invalid argument: 'Scaling'. Accepted arguments are: quantile,
    studentized, asymmetric, none.")
  }
  # Verify Method
  if(!is.character(Method) && !is.vector(Method) && !length(Method) == 1) {
    stop("Argument 'Method' must be a character vector of length one")
  }
  if(!(Method %in% c("Integral", "Supremum"))) {
    stop("Invalid argument: 'Method'. Accepted arguments are: Integral, Supremum.")
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
  rIncrements <- (r - c(0, r)[seq_along(r)])[-1]

  # Calculate the weights to scale the residuals of the statistic
  if(Scaling == "studentized") {
    Weights <- 1/apply(SimulatedValues, 1, sd, na.rm = T)
  } else if (Scaling == "quantile") {
    Weights <- 1/(apply(SimulatedValues, 1, quantile, probs = 0.975, na.rm = T)-
                    apply(SimulatedValues, 1, quantile, probs = 0.025, na.rm = T))
  } else if (Scaling == "asymmetric") {
    Upper <- 1/(apply(SimulatedValues,1,quantile,probs = 0.975,na.rm = T)-
                  AverageSimulatedValues)
    Lower <- 1/(AverageSimulatedValues-
                  apply(SimulatedValues, 1, quantile, probs = 0.025, na.rm = T))
    Weights <- list(UprW = Upper, LwrW = Lower)
  } else {
    Weights <- rep(1,length(r))
  }

  # Ui calculate the statistic for a simulation
  Ui <- function(SimulationNumber) {
    Departure <- (SimulatedValues[, SimulationNumber] -
                    AverageSimulatedValues)[seq_along(r) - 1]
    if(inherits(Weights, "list")) {
      ScaledDeparture <- sapply(seq_along(Departure),
                                FUN= function(x) ifelse(Departure[x]>=0,
                                                        Departure[x]*Weights$UprW[x],
                                                        Departure[x]*Weights$LwrW[x]))
      ScaledDeparture <- as.vector(ScaledDeparture)
    } else {
      ScaledDeparture <- Departure*Weights[seq_along(r) - 1]
    }
    if(Method == "Integral") {
      GofStatistic <- sum((ScaledDeparture[!is.nan(ScaledDeparture)])^2 *
                            rIncrements[!is.nan(ScaledDeparture)], na.rm = T)
    } else if(Method == "Supremum") {
      GofStatistic <- max(ScaledDeparture, na.rm = T)
    }
    return(GofStatistic)
  }

  # Calculate the Ui statistic for all simulations
  SimulatedU <- vapply(
    seq_len(NumberOfSimulations),
    FUN = Ui,
    FUN.VALUE = 0
  )

  # Calculate the Ui statistic for the actual value
  ResidualValues <- (ActualValues - AverageSimulatedValues)[seq_along(r) - 1]
  if(inherits(Weights, "list")) {
    ScaledResidualValues <- sapply(seq_along(ResidualValues),
                                   FUN= function(x) ifelse(ResidualValues[x]>=0,
                                                           ResidualValues[x]*Weights$UprW[x],
                                                           ResidualValues[x]*Weights$LwrW[x]))
    ScaledResidualValues <- as.vector(ScaledResidualValues)
  } else {
    ScaledResidualValues <- ResidualValues*Weights[seq_along(r) - 1]
  }
  if(Method == "Integral") {
    ActualU <- sum((ScaledResidualValues[!is.nan(ScaledResidualValues)])^2 *
                     rIncrements[!is.nan(ScaledResidualValues)], na.rm = T)
  } else if(Method == "Supremum") {
    ActualU <- max(ScaledResidualValues, na.rm = T)
  }

  # Return the rank
  return(mean(ActualU < SimulatedU))
}
