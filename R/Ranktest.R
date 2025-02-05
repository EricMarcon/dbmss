Ranktest = function(
    Envelope,
    Tail = "two-tailed",
    Method = "RankCountOrdering") {

  # Verify Envelope
  if (!inherits(Envelope, "envelope")) {
    stop("Envelope is not of class envelope")
  }

  # Verify Tail
  if (!is.character(Tail) | !is.vector(Tail) | !length(Tail) == 1) {
    stop("Argument 'Tail' must be a character vector of length one")
  }
  if (!(Tail %in% c("two-tailed", "left-tailed", "right-tailed"))) {
    stop("Invalid argument: 'Tail'. Accepted arguments are: two-tailed,
    left-tailed, right-tailed.")
  }

  # Verify Method
  if (!is.character(Method) | !is.vector(Method) | !length(Method) == 1) {
    stop("Argument 'Method' must be a character vector of length one")
  }
  if (!(Method %in% c("RankCountOrdering", "SimpleRankOrdering"))) {
    stop("Invalid argument: 'Method'. Accepted arguments are: RankCountOrdering,
    SimpleRankOrdering.")
  }

  # Verify simulations
  if (is.null(attr(Envelope, "simfuns"))) {
    stop("Envelope does not contain simulations in its attribute simfuns")
  } else {
    ActualValues <- Envelope$obs
    SimulatedValues <- as.data.frame(attr(Envelope, "simfuns"))[, -1]
  }

  # Combine actual and simulated values into one data set
  AllValues <- cbind(SimulatedValues, ActualValues)

  # Remove values at d = 0 (many NaNs)
  AllValues <- AllValues[-1, ]

  # Compute function ranks for all simulations and the actual value
  Rankmatrix <- rbind(apply(AllValues, 1, rank, na.last = NA))
  Rankmatrix <- t(Rankmatrix)
  if (Tail == "right-tailed") {
    Rankmatrix <- dim(Rankmatrix)[2] - Rankmatrix
  }
  if (Tail == "two-tailed") {
    Rankmatrix <- rbind(dim(Rankmatrix)[2] - Rankmatrix, Rankmatrix)
  }

  # Compute Extreme Rank Ordering Test
  if (Method == "SimpleRankOrdering") {
    Ri <- apply(Rankmatrix, 2, min, na.rm = T)
    Liberalp <- mean(Ri[names(Ri) != "ActualValues"] < Ri["ActualValues"])
    Conservativep <- mean(Ri[names(Ri) != "ActualValues"] <= Ri["ActualValues"])
  }

  # Compute Extreme Rank Count Ordering Test (attempt to break ties)
  if (Method == "RankCountOrdering") {
    OrderedRank <- apply(Rankmatrix,
                         2,
                         FUN = function(x) x[order(x, na.last = NA)])
    ObservedRank <- OrderedRank[, "ActualValues"]
    LiberalRank <- sum(OrderedRank[1, colnames(OrderedRank) != "ActualValues"] <
                         ObservedRank[1])
    TieMatrixColumn <- c(OrderedRank[1, colnames(OrderedRank) != "ActualValues"] ==
                           ObservedRank[1], FALSE)
    TieSimulation <- OrderedRank[, TieMatrixColumn, drop = F]

    # Break ties between the actual values and equivalent simulation
    if (dim(TieSimulation)[2] == 0) {
      ConservativeRank <- LiberalRank
    } else {
      Rowi <- 2
      while (any(TieSimulation[Rowi, ] >= ObservedRank[Rowi]) &&
            Rowi < length(ObservedRank)) {
        TieSimulation <- TieSimulation[, !(TieSimulation[Rowi, ] > ObservedRank[Rowi]),
                                       drop = F]
        Rowi <- Rowi + 1
      }
      ConservativeRank <- LiberalRank + dim(TieSimulation)[2]
    }
    Liberalp <- LiberalRank / dim(SimulatedValues)[2]
    Conservativep <- ConservativeRank / dim(SimulatedValues)[2]
  }
  return(c(Liberalp, Conservativep))
}
