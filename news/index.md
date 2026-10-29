# Changelog

## dbmss 2.11-0

CRAN release: 2025-06-17

### Improvements

- Extended
  [`GoFtest()`](https://ericmarcon.github.io/dbmss/reference/GoFtest.md)
  following Myllymäki et al. (2015).

## dbmss 2.10-0

CRAN release: 2025-05-04

### Improvements

- parallelized `xxxEnvelope` code.
- improved the estimation of the g function in
  [`ghat()`](https://ericmarcon.github.io/dbmss/reference/ghat.md):
  - bandwidth selection now follows exactly Stoyan and Stoyan, 1994.
  - the max distance is no longer censored, for consistency with other
    functions.
  - the estimation was not correct at last distances.
- allowed setting the precision of simulated coordinates in
  [`gEnvelope()`](https://ericmarcon.github.io/dbmss/reference/gEnvelope.md).
- sorted point types in decreasing order of abundances in
  [`autoplot.wmppp()`](https://ericmarcon.github.io/dbmss/reference/autoplot.md).
- avoided accessing `ppp` objects’ marks directly.
- cleaned up the code

## dbmss 2.9-2

CRAN release: 2024-08-24

### Improvements

- pkgdown site declared in DESCRIPTION URL.
- figure width is set to 5cm in the vignette to make the figures
  readable.

## dbmss 2.9-0

CRAN release: 2023-12-11

### Improvements

- NAMESPACE cleaned up.

### Significant user-visible changes

- [`Smooth.wmppp()`](https://ericmarcon.github.io/dbmss/reference/Smooth.wmppp.md)
  smooths wmppp’s to map individual values of functions such as $`M`$ in
  the neighborhood of points.

## dbmss 2.8-2

CRAN release: 2023-05-21

### Improvements

- C code cleaned up.

### External changes

- [`Dhat()`](https://ericmarcon.github.io/dbmss/reference/Dhat.md) code
  modified according to *spatstat.explore* v.3.2-1 new features (thanks
  to Prof. Adrian Baddeley).

## dbmss 2.8-0

CRAN release: 2023-01-06

### Significant user-visible changes

- `kwmppp()` creates a kriged, weighted, marked, planar point pattern to
  map individual distance-based statistics.
- `plot.kwmppp()` (called as
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)) allows
  plotting `kwmppp` objects.
- argument `verbose` in all envelope functions allows to hide the
  progress of simulations.

## dbmss 2.7-11

CRAN release: 2022-11-08

### Improvements

- [`Ktest()`](https://ericmarcon.github.io/dbmss/reference/Ktest.md)
  automatically drops 0 in `r` to avoid an obscure error message.

### External changes

- Updates in the *spatstat* package: *dbmss* has been updated to address
  the replacement of *spatstat.core*.

## dbmss 2.7-10

CRAN release: 2022-07-10

### Improvements

- dbmss no longer depends on ggplot2.
- Deprecated `aes_(x=~var)` aesthetics in ggplots replaced by
  `aes(x=.data$var)`.
- documentation improvements.

## dbmss 2.7-8

CRAN release: 2022-01-25

### External changes

- The new package spatstat.random is now imported.

## dbmss 2.7-7

CRAN release: 2021-08-22

### External changes

- Following R fixed bug
  [PR#18151](https://bugs.r-project.org/show_bug.cgi?id=18151),
  [`mhat()`](https://ericmarcon.github.io/dbmss/reference/m_hat.md) code
  is modified by Martin Maechler.

## dbmss 2.7-6

CRAN release: 2021-06-04

### Bug correction

- An error occurred when a function was called with `dbmss::` prefix and
  `CheckArguments = TRUE` and dbmss was loaded.

## dbmss 2.7-5

CRAN release: 2021-04-16

### Improvements

- `fmla` argument added in
  [`autoplot.envelope()`](https://ericmarcon.github.io/dbmss/reference/autoplot.md).
- [`autoplot.fv()`](https://ericmarcon.github.io/dbmss/reference/autoplot.md)
  added.
- [`autoplot.wmppp()`](https://ericmarcon.github.io/dbmss/reference/autoplot.md)
  added.
- CodeFactor optimization applied.
- Suggests rmarkdown (<https://github.com/yihui/knitr/issues/1864>)

## dbmss 2.7-4

CRAN release: 2021-01-25

### Improvements

- Continuous integration by GitHub Actions.

### External changes

- Updates in the *spatstat* package: *dbmss* has been updated to address
  the creation of *spatstat.core* et al.

## dbmss 2.7-3

CRAN release: 2020-11-01

### Bug fixes

- argument `main` in
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  was ignored.

## dbmss 2.7-1

CRAN release: 2020-01-08

### Improvements

- tibbles are accepted by
  [`wmppp()`](https://ericmarcon.github.io/dbmss/reference/wmppp.md).
- Precision of point coordinates can be set in
  [`rRandomPositionK()`](https://ericmarcon.github.io/dbmss/reference/rRandomPositionK.md).

### Bug fixes

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  failed with `Lenvelope` objects.

## dbmss 2.7-0

CRAN release: 2019-03-25

### Improvements

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for `envelope` objects.
- Vignettes by [pkgdown](https://pkgdown.r-lib.org/)
- Better consistency of bandwidth choice in `Kd` and `m` estimation. An
  article is added in the online documentation.

### Internal changes

- Fewer imported functions in NAMESPACE.

### Bug fixes

- [`Kdhat()`](https://ericmarcon.github.io/dbmss/reference/Kdhat.md)
  density was normalized incorrectly with Approximate=TRUE and small r
  values.

## dbmss 2.6-3

CRAN release: 2018-03-19

### Improvements

- More robust check of arguments.
- Rcpp \>= 0.12.14 required. `init.c` no longer necessary.
- Introduction vignette.
- On Travis and codecov.io now.
- NEWS in Markdown.
- [`wmppp()`](https://ericmarcon.github.io/dbmss/reference/wmppp.md)
  accepts characters in `PointType`. A `PointName` column is searched to
  set the row names of the marks.

### Bug fixes

- [`wmppp()`](https://ericmarcon.github.io/dbmss/reference/wmppp.md)
  failed if some point laid outside the window.

## dbmss 2.5-1

CRAN release: 2017-11-28

### New features

- On GitHub now.
- Individual values of m and M available from `Mhat` and `mhat` with
  argument `Individual`.
- Point names can be specified as row names of the dataframe passed to
  [`wmppp()`](https://ericmarcon.github.io/dbmss/reference/wmppp.md).
  They are preserved in the `fv` object returned by individual `Mhat`
  and `mhat` to identify points more easily.

### Improvements

- Far less memory is used to compute M and approximated m functions. The
  ratio of necessary memory equals that of the number of reference
  points to the total number of points.
- `[.wmppp` function added to *spatstat* generics

## dbmss 2.4-1

### Improvements

- Declaration of the required versions of R and *spatstat* in
  DESCRIPTION to avoid error in CRAN test on oldrel Windows platform.

## dbmss 2.4-0

CRAN release: 2017-03-26

### External changes

- Updates in the *spatstat* package: *dbmss* has been updated to address
  the creation of *spatstat.util*.

### Internal changes

- C routines registration to comply with R 3.4 policy.
- Explicit export of all non-internal functions instead of
  `exportPattern("^[[:alpha:]]+")`.
- Import of package *cubature* reduced to function `adaptIntegrate()`.

## dbmss 2.3-0

CRAN release: 2016-09-19

### Internal changes

- Enforcement of the use of C++11 to avoid warnings due to
  *RcppParallel*.

## dbmss 2.2-5

CRAN release: 2016-06-30

### Significant user-visible changes

- Distance matrices can be used instead of point patterns in `Mhat`,
  `mhat` and `Kdhat` and their envelopes.
- The estimation of density used by `Kd` now includes reflection: the
  estimation of `Kd` is more accurate than before, but values may vary
  from previous versions close to the minimum distance.
- LazyData is used to save memory.

### Bug fixes

- Weighted argument was ignored in `Kdhat` (bug introduced in v.2.2-4).
  Fixed.

## dbmss 2.2-4

CRAN release: 2015-12-10

### Significant user-visible changes

- `Mhat` and `mhat` C++ code is now parallelized thanks to
  *RcppParallel*.
- Small performance improvement of `Kdhat` (around 5% faster).

## dbmss 2.2-3

### Significant user-visible changes

- Updated CITATION: the paper about this package has been published:
  Eric Marcon, Stephane Traissac, Florence Puech, Gabriel Lang (2015).
  Tools to Characterize Point Patterns: dbmss for R. Journal of
  Statistical Software, 67(3), 1-15.

## dbmss 2.2-2

### Minor change

- Intervals of estimation of the Kd function are narrower when the
  distance range is small with respect to the window to improve
  precision.

### Bug fixes

- Distances pairs more than twice the maximum value of `r` where ignored
  when using the approximate estimation of `Kd`, so `Kd` was
  overestimated when `r` was smaller than usual (much less than the
  default values). Corrected.

## dbmss 2.2-1

### Bug fixes

- Adjust argument ignored in `Kdhat`. Fixed.

## dbmss 2.2-0

### Significant user-visible changes

- `m` function added.
- Default value `Controls=NULL` for `Dhat`: controls are by default all
  points except for cases.
- `Kd` and `m` are both computed by default up to one third of the
  diameter of the window, other options are added.
- `Original` and `Adjust` arguments allow to change the default bandwith
  used by `Kd` and `m`.

### Minor change

- Typo in internal function name `FillEnveloppe` corrected:
  `FillEnvelope` is the new name.

## dbmss 2.1-2

### Significant user-visible changes

- Default neighbor type for `Khat` and `Kenvelope` is the same as
  reference type instead of ““.
- `Kd` estimation now accepts “” as reference type to use all points.
- Approximate argument to calculate `Kd` on big data sets with little
  RAM.

### Bug fixes

- `FillEnvelope` returned wrong quantiles for local confidence
  intervals. Fixed.

## dbmss 2.1-1

### Significant user-visible changes

- Global confidence intervals are now defined even when `NA` values are
  found in the simulations.
- `Kd` is computed by default up to the median (instead of mean)
  distance between points.

### Bug fixes

- `Kdhat` with Reference and Neighbor points of different types was
  calculated with only half of point pairs and sometimes crashed R.
  Fixed.

## dbmss 2.1-0

### Significant user-visible changes

- `Kdhat` and `M` are computed through C++ loops instead of spatstat
  `pairdist()`. Much faster, and uses far less memory.

## dbmss 2.0-6

### Significant user-visible changes

- `Kdhat` and `M` propose a default value for argument `r`.
- Vignette added.

### Minor change

- `Kdhat` probability density estimation is cut at the lowest distance
  between points instead of 0.

## dbmss 2.0-5

### Bug fixes

- `Kdhat` with Reference and Neighbor points of the same type and
  `Weighted=TRUE` returned an error. Fixed.

## dbmss 2.0-3

### Overview

- Minor corrections, mainly comments and formating.

## dbmss 2.0-0

### Overview

- Most code rewritten.

### Significant user-visible changes

- Names of functions `X.r` became Xhat.
- Point patterns are now of class `wmppp`.
- Results are of class `fv`, envelopes of class `envelope`.

## dbmss 1.2-5

### Overview

- NEWS file added.

## dbmss 1.2-4

### Overview

- Documentation format improved.

### Significant user-visible changes

- The default behavior of `Kd` is that of Duranton and Overman (2005).
  An optional parameter is added to used improved bandwith selection.

### Bug fixes

- The density estimation of `Kd` was run twice. Fixed.

## dbmss 1.2-3

### \## Overview

- First version on CRAN. Versions 1.2.2 and 1.2.3 contain faster
  examples to follow CRAN requirements.

## dbmss 1.2-1

### Overview

- `Kinhom` function added.

## dbmss 1.1

### Overview

- First version.
