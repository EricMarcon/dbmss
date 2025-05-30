GoFtest <- function(
    Envelope,
    Test = "DCLF",
    Scaling = "Asymmetric",
    Range = NULL,
    Alpha = 0.05,
    CheckArguments = TRUE) {

  if (CheckArguments) {
    CheckdbmssArguments()
  }

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("'Envelope' must be of class 'envelope'")
  }
  # Verify Scaling
  if (!is.character(Scaling) | !is.vector(Scaling) | !length(Scaling) == 1) {
    stop("Argument 'Scaling' must be a character vector of length one")
  }
  if (!(Scaling %in% c("Quantile", "Studentized", "Asymmetric", "None"))) {
    stop(
     "Invalid argument: 'Scaling'. Accepted arguments are: Quantile,
     Studentized, Asymmetric, None."
    )
  }
  # Verify Test
  if (!is.character(Test) | !is.vector(Test) | !length(Test) == 1) {
    stop("Argument 'Test' must be a character vector of length one")
  }
  if (!(Test %in% c("DCLF", "Integral", "MAD"))) {
    stop(
      "Invalid argument: 'Test'. Accepted arguments are: DCLF, Integral, MAD."
    )
  }
  # Verify Range
  if (
    !is.null(Range) &&
    (!is.vector(Range) | !is.numeric(Range) | length(Range) != 2)
  ) {
    stop(
      "Invalid argument: 'Range'. Accepted arguments are a numeric vector of length two,
      specifying the minimum and maximum distances over which to compute the test."
    )
  }
  # Verify simulations
  if (is.null(attr(Envelope, "simfuns"))) {
    stop("Envelope does not contain simulations in its attribute simfuns")
  } else {
    r <- as.data.frame(attr(Envelope, "simfuns"))[, 1]
    ActualValues <- Envelope$obs
    SimulatedValues <- as.data.frame(attr(Envelope, "simfuns"))[, -1]
  }

  # Transform observed L's into K for the test
  # (L envelopes are constructed from the K function)
  if (any(attr(Envelope, "fname") %in% c("L", "Lmm"))) {
    ActualValues <- (ActualValues + r)^2 * pi
  }

  # Restrict analysis to chosen distance range
  if (!is.null(Range)) {
    if (min(Range) < max(r) && max(Range) > min(r)) {
      SelectedR <- (r > min(Range) & r < max(Range))
      ActualValues <- ActualValues[SelectedR]
      SimulatedValues <- SimulatedValues[SelectedR, ]
      r <- r[SelectedR]
    } else {
      warning(
       "The selected range is outside the simulated distances.
       The test was computed using all distances from the envelope."
      )
    }
  }

  NumberOfSimulations <- dim(SimulatedValues)[2]
  AverageSimulatedValues <- apply(
    SimulatedValues,
    MARGIN = 1,
    FUN = sum
  ) / (NumberOfSimulations - 1)
  rIncrements <- (r - c(0, r)[seq_along(r)])[-1]

  # Calculate weights to scale residuals of the statistic
  Weights <- switch(
    Scaling,
    "Studentized" = 1 / apply(
      SimulatedValues,
      MARGIN = 1,
      FUN = sd,
      na.rm = TRUE
    ),
    "Quantile" = 1 / (
      apply(
        SimulatedValues,
        MARGIN =  1,
        FUN = quantile,
        probs = 1 - Alpha / 2,
        na.rm = TRUE
      ) - apply(
        SimulatedValues,
        MARGIN = 1,
        FUN = quantile,
        probs = Alpha / 2,
        na.rm = TRUE
      )
    ),
    "Asymmetric" = {
      Upper <- 1 / (
        apply(
          SimulatedValues,
          MARGIN = 1,
          FUN = quantile,
          probs = 1 - Alpha / 2,
          na.rm = TRUE
        ) - AverageSimulatedValues
      )
      Lower <- 1 / (
        AverageSimulatedValues - apply(
          SimulatedValues,
          MARGIN = 1,
          FUN = quantile,
          probs = Alpha / 2,
          na.rm = TRUE
        )
      )
      list(UprW = Upper, LwrW = Lower)
    },
    "None" = rep(1, length(r))
  )

  # Ui calculate the statistic for one simulation
  Ui <- function(SimulationNumber, ValueToTest) {
    Departure <- (
      ValueToTest[, SimulationNumber] - AverageSimulatedValues
    )[seq_along(r) - 1]
    if (inherits(Weights, "list")) {
      ScaledDeparture <- sapply(
        seq_along(Departure),
        FUN = function(x) {
          ifelse(
            Departure[x] >= 0,
            Departure[x] * Weights$UprW[x],
            Departure[x] * Weights$LwrW[x]
          )
        }
      )
      ScaledDeparture <- as.vector(ScaledDeparture)
    } else {
      ScaledDeparture <- Departure * Weights[seq_along(r) - 1]
    }
    GofStatistic <- switch(
      Test,
      "DCLF" = sum(
        (ScaledDeparture[!is.nan(ScaledDeparture)])^2 *
          rIncrements[!is.nan(ScaledDeparture)],
        na.rm = TRUE
      ),
      "Integral" = sum(
        abs((ScaledDeparture[!is.nan(ScaledDeparture)])) *
          rIncrements[!is.nan(ScaledDeparture)],
        na.rm = TRUE
      ),
      "MAD" = max(abs(ScaledDeparture), na.rm = TRUE))
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

  # Return the p_value. If the p_value is equal to 0, a conservative p_value
  # of 1/(n + 1) is returned.
  return(
    ifelse(
      mean(ActualU < SimulatedU) == 0,
      1 / (1 + NumberOfSimulations),
      mean(ActualU < SimulatedU)
    )
  )
}
