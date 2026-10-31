# Computes simulation envelopes of a summary function.

Prints a useful summary of a confidence envelope of class
"dbmssEnvelope"

## Usage

``` r
# S3 method for class 'Dtable'
envelope(Y, fun = Kest, nsim = 99, nrank = 1, ..., 
          funargs = list(), funYargs = funargs, simulate = NULL, 
          verbose = TRUE, savefuns = FALSE, Yname = NULL, envir.simul = NULL)
```

## Arguments

- Y:

  An object of class
  [`Dtable`](https://ericmarcon.github.io/dbmss/dev/reference/Dtable.md).

- fun:

  Function that computes the desired summary statistic for `Y`.

- nsim:

  Number of simulated point patterns to be generated when computing the
  envelopes.

- nrank:

  Integer. Rank of the envelope value amongst the nsim simulated values.
  A rank of 1 means that the minimum and maximum simulated values will
  be used.

- ...:

  Extra arguments passed to `fun`.

- funargs:

  A list, containing extra arguments to be passed to `fun`.

- funYargs:

  Optional. A list, containing extra arguments to be passed to fun when
  applied to the original data `Y` only.

- simulate:

  Optional. Specifies how to generate the simulated point patterns.

- verbose:

  Logical flag indicating whether to print progress reports during the
  simulations.

- savefuns:

  Logical flag indicating whether to save all the simulated function
  values.

- Yname:

  Character string that should be used as the name of the data `Y` when
  printing or plotting the results.

- envir.simul:

  Environment in which to evaluate the expression `simulate`, if not the
  current environment.

## Details

This is the S3 method
[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html) for
[`Dtable`](https://ericmarcon.github.io/dbmss/dev/reference/Dtable.md)
objects.

## Author

Eric Marcon \<Eric.Marcon@agroparistech.fr\>. Relies on the
[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)
engine of **spatstat**.
