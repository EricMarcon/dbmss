envelope.Dtable <-
function (Y, fun = Kest, nsim = 99, nrank = 1, ..., funargs = list(), 
          funYargs = funargs, simulate = NULL, verbose = TRUE, savefuns = FALSE,
          Yname = NULL, envir.simul = NULL) 
{
  # Environment information sent to envelopeEngine
  cl <- spatstat.utils::short.deparse(sys.call())
  if (is.null(Yname)) 
    Yname <- spatstat.utils::short.deparse(substitute(Y))
  if (is.null(fun)) 
    fun <- Kest
  envir.user <- if (!is.null(envir.simul)) 
    envir.simul
  else parent.frame()
  envir.here <- sys.frame(sys.nframe())

  if (is.null(simulate)) {
    stop("The simulation function must be provided in the simulate argument.")
  } else {
    simrecipe <- simulate
    X <- Y
  }

  # Run the simulations  
  envelopeEngine(X = X, fun = fun, simul = simrecipe, nsim = nsim, 
                 nrank = nrank, ..., funargs = funargs, funYargs = funYargs, 
                 verbose = verbose, clipdata = FALSE, transform = NULL, 
                 global = FALSE, ginterval = NULL, use.theory = NULL, 
                 alternative = c("two.sided", "less", "greater"), scale = NULL, 
                 clamp = FALSE, 
                 savefuns = savefuns, savepatterns = FALSE, nsim2 = nsim, 
                 VARIANCE = FALSE, nSD = 2, Yname = Yname, maxnerr = nsim, 
                 cl = cl, envir.user = envir.user, do.pwrong = FALSE,
                 foreignclass = "Dtable")
}