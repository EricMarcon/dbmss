# Estimation of the confidence envelope of the M function under its null hypothesis

Simulates point patterns according to the null hypothesis and returns
the envelope of *M* according to the confidence level.

## Usage

``` r
MEnvelope(X, r = NULL, NumberOfSimulations = 100, Alpha = 0.05,
          ReferenceType, NeighborType = ReferenceType,
          CaseControl = FALSE, SimulationType = "RandomLocation", Global = FALSE,
          verbose = interactive(), parallel = FALSE, parallel_pgb_refresh = 1/10)
```

## Arguments

- X:

  A point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md))
  or a
  [`Dtable`](https://ericmarcon.github.io/dbmss/dev/reference/Dtable.md)
  object.

- r:

  A vector of distances. If `NULL`, a default value is set: 32 unequally
  spaced values are used up to half the maximum distance between points
  \\d_m\\. The first value is 0, first steps are small (\\d_m/200\\)
  then incresase progressively up to \\d_m/20\\.

- NumberOfSimulations:

  The number of simulations to run, 100 by default.

- Alpha:

  The risk level, 5% by default.

- ReferenceType:

  One of the point types.

- NeighborType:

  One of the point types, equal to the reference type by default to
  caculate univariate M.

- CaseControl:

  Logical; if `TRUE`, the case-control version of *M* is computed.
  *ReferenceType* points are cases, *NeighborType* points are controls.

- SimulationType:

  A string describing the null hypothesis to simulate. The null
  hypothesis may be "*RandomLocation*": points are redistributed on the
  actual locations (default); "*RandomLabeling*": randomizes point
  types, keeping locations and weights unchanged;
  "*PopulationIndependence*": keeps reference points unchanged,
  randomizes other point locations.

- Global:

  Logical; if `TRUE`, a global envelope sensu Duranton and
  Overman (2005) is calculated.

- verbose:

  Logical; if `TRUE`, print progress reports during the simulations.

- parallel:

  Logical; if `TRUE`, simulations can be run in parallel, see details.

- parallel_pgb_refresh:

  The proportion of simulations steps to be displayed by the parallel
  progress bar. 1 will show all but may slow down the computing, 1/100
  only one out of a hundred.

## Details

This envelope is local by default, that is to say it is computed
separately at each distance. See Loosmore and Ford (2006) for a
discussion.

The global envelope is calculated by iteration: the simulations reaching
one of the upper or lower values at any distance are eliminated at each
step. The process is repeated until *Alpha / Number of simulations*
simulations are dropped. The remaining upper and lower bounds at all
distances constitute the global envelope. Interpolation is used if the
exact ratio cannot be reached.

Parallel simulations rely on the *future* and *doFuture* packages.
Before calling the function with argument `parallel = TRUE`, you must
choose a strategy and set it with
[`plan`](https://future.futureverse.org/reference/plan.html). Their
progress bar relies on the *progressr* package. They must be activated
by the user by
[`handlers`](https://progressr.futureverse.org/reference/handlers.html).

## Value

An envelope object
([`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)).
There are methods for print and plot for this class.

The `fv` contains the observed value of the function, its average
simulated value and the confidence envelope.

## References

Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using
Micro-Geographic Data. *Review of Economic Studies* 72(4): 1077-1106.

Kenkel, N. C. (1988). Pattern of Self-Thinning in Jack Pine: Testing the
Random Mortality Hypothesis. *Ecology* 69(4): 1017-1024.

Loosmore, N. B. and Ford, E. D. (2006). Statistical inference using the
G or K point pattern spatial statistics. *Ecology* 87(8): 1925-1931.

Marcon, E. and F. Puech (2017). A typology of distance-based measures of
spatial concentration. *Regional Science and Urban Economics*. 62:56-67.

## See also

[`Mhat`](https://ericmarcon.github.io/dbmss/dev/reference/Mhat.md)

## Examples

``` r
data(paracou16)
# Keep only 50% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
autoplot(X,
  labelSize = expression("Basal area (" ~cm^2~ ")"),
  labelColor = "Species")


# Calculate confidence envelope (should be 1000 simulations, reduced to 4 to save time)
NumberOfSimulations <- 4
Alpha <- .10
autoplot(MEnvelope(X, , NumberOfSimulations, Alpha,
    "V. Americana", "Q. Rosea", FALSE, "RandomLabeling"))
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_ribbon()`).
```
