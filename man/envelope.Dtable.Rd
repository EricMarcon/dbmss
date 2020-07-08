\name{envelope.Dtable}
\alias{envelope.Dtable}
\title{
  Computes simulation envelopes of a summary function.
}
\description{
  Prints a useful summary of a confidence envelope of class "dbmssEnvelope"
}
\usage{
\method{envelope}{Dtable}(Y, fun = Kest, nsim = 99, nrank = 1, ..., 
          funargs = list(), funYargs = funargs, simulate = NULL, 
          verbose = TRUE, savefuns = FALSE, Yname = NULL, envir.simul = NULL)
}
\arguments{
  \item{Y}{
  An object of class \code{\link{Dtable}}.
  }
  \item{fun}{
  Function that computes the desired summary statistic for \code{Y}.
  }
  \item{nsim}{
  Number of simulated point patterns to be generated when computing the envelopes.
  }
  \item{nrank}{
  Integer. Rank of the envelope value amongst the nsim simulated values. A rank of 1 means that the minimum and maximum simulated values will be used.
  }
  \item{\dots}{
  Extra arguments passed to \code{fun}.
  }
  \item{funargs}{
  A list, containing extra arguments to be passed to \code{fun}.
  }
  \item{funYargs}{
  Optional. A list, containing extra arguments to be passed to fun when applied to the original data \code{Y} only.
  }
  \item{simulate}{
  Optional. Specifies how to generate the simulated point patterns.
  }
  \item{verbose}{
  Logical flag indicating whether to print progress reports during the simulations.
  }
  \item{savefuns}{
  Logical flag indicating whether to save all the simulated function values.
  }
  \item{Yname}{
  Character string that should be used as the name of the data \code{Y} when printing or plotting the results.
  }
  \item{envir.simul}{
  Environment in which to evaluate the expression \code{simulate}, if not the current environment.
  }
}
\details{
  This is the S3 method \code{\link{envelope}} for \code{\link{Dtable}} objects.
}
\author{
  Eric Marcon <Eric.Marcon@ecofog.gf>. Relies on the \code{\link{envelope}} engine of \strong{spatstat}.
}
