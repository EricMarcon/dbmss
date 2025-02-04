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
    Method = "integral") {

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("Envelope is not of class envelope")
  }
  # Verify Scaling
  if(!is.character(Scaling) && !is.vector(Scaling) && !length(Scaling) == 1) {
    stop("Argument 'Scaling' must be a character vector of length one")
  }
  if(!(Scaling %in% c("quantile","studentized","asymmetric","none"))) {
    stop("Invalid argument: 'Scaling'. Accepted arguments are: quantile,
    studentized, asymmetric, none.")
  }
  # Verify Method
  if(!is.character(Method) && !is.vector(Method) && !length(Method) == 1) {
    stop("Argument 'Method' must be a character vector of length one")
  }
  if(!(Method %in% c("integral","supremum"))) {
    stop("Invalid argument: 'Method'. Accepted arguments are: integral, supremum.")
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
    Weights[!(is.finite(Weights))] <- 0
  } else if (Scaling == "quantile") {
    Weights <- 1/(apply(SimulatedValues, 1, quantile, probs = 0.975, na.rm = T)-
                    apply(SimulatedValues, 1, quantile, probs = 0.025, na.rm = T))
    Weights[!(is.finite(Weights))]<- 0
  } else if (Scaling == "asymmetric") {
    Upper <- 1/(apply(SimulatedValues,1,quantile,probs = 0.975,na.rm = T)-
                  AverageSimulatedValues)
    Lower <- 1/(AverageSimulatedValues-
                  apply(SimulatedValues, 1, quantile, probs = 0.025, na.rm = T))
    Upper[!(is.finite(Upper))] <- 0
    Lower[!(is.finite(Lower))] <- 0
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
    if(Method == "integral") {
      GoFstatistic <- sum((ScaledDeparture[!is.nan(ScaledDeparture)])^2 *
                            rIncrements[!is.nan(ScaledDeparture)], na.rm = T)
    } else if(Method == "supremum") {
      GoFstatistic <- max(ScaledDeparture, na.rm = T)
    }
    return(GoFstatistic)
  }

  # Merge all simulations and the actual value to calculate the Ui statistics
  SimulatedValues <- cbind(SimulatedValues, ActualValues)
  NewNumberOfSimulations <- dim(SimulatedValues)[2]

  # Calculate the Ui statistic for all simulations and the actual value
  SimulatedU <- vapply(
    seq_len(NewNumberOfSimulations),
    FUN = Ui,
    FUN.VALUE = 0
  )

  # Extract the Ui statistic of the actual value
  ActualU <- SimulatedU[length(SimulatedU)]

  # Return the rank
  return(mean(ActualU < SimulatedU[-length(SimulatedU)]))
}
