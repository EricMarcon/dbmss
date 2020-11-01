# dbmss 2.7-3

## Bug fixes

- argument `main` in `autoplot()` was ignored.


# dbmss 2.7-1

## Improvements

- tibbles are accepted by `wmppp()`.
- Precision of point coordinates can be set in `rRandomPositionK()`.

## Bug fixes

- `autoplot()` failed with `Lenvelope` objects.



# dbmss 2.7-0

## Improvements

- `autoplot()` for `envelope` objects.
- Vignettes by [pkgdown](https://pkgdown.r-lib.org/)
- Better consistency of bandwidth choice in `Kd` and `m` estimation. An article is added in the online documentation.

## Internal changes

- Fewer imported functions in NAMESPACE.

## Bug fixes

- `Kdhat()` density was normalized incorrectly with Approximate=TRUE and small r values.



# dbmss 2.6-3

## Improvements

- More robust check of arguments.
- Rcpp >= 0.12.14 required. `init.c` no longer necessary. 
- Introduction vignette.
- On Travis and codecov.io now.
- NEWS in Markdown.
- `wmppp()` accepts characters in `PointType`. A `PointName` column is searched to set the row names of the marks.

## Bug fixes

- `wmppp()` failed if some point laid outside the window.



# dbmss 2.5-1

## New features

- On GitHub now.
- Individual values of m and M available from `Mhat` and `mhat` with argument `Individual`.
- Point names can be specified as row names of the dataframe passed to `wmppp()`. They are preserved in the `fv` object returned by individual `Mhat` and `mhat` to identify points more easily.

## Improvements

- Far less memory is used to compute M and approximated m functions. The ratio of necessary memory equals that of the number of reference points to the total number of points.
- `[.wmppp` function added to _spatstat_ generics



# dbmss 2.4-1

## Improvements

- Declaration of the required versions of R and _spatstat_ in DESCRIPTION to avoid error in CRAN test on oldrel Windows platform.
  

  
# dbmss 2.4-0

## External changes

- Updates in the _spatstat_ package: _dbmss_ has been updated to address the creation of _spatstat.util_.

## Internal changes

- C routines registration to comply with R 3.4 policy.
- Explicit export of all non-internal functions instead of `exportPattern("^[[:alpha:]]+")`.
- Import of package _cubature_ reduced to function `adaptIntegrate()`.


  
# dbmss 2.3-0

## Internal changes

- Enforcement of the use of C++11 to avoid warnings due to _RcppParallel_.

  

# dbmss 2.2-5

## Significant user-visible changes

- Distance matrices can be used instead of point patterns in `Mhat`, `mhat` and `Kdhat` and their envelopes.
- The estimation of density used by `Kd` now includes reflection: the estimation of `Kd` is more accurate than before, but values may vary from previous versions close to the minimum distance.
- LazyData is used to save memory.

## Bug fixes

- Weighted argument was ignored in `Kdhat` (bug introduced in v.2.2-4). Fixed.



# dbmss 2.2-4

## Significant user-visible changes

- `Mhat` and `mhat` C++ code is now parallelized thanks to _RcppParallel_.
- Small performance improvement of `Kdhat` (around 5% faster).
  
  
  
# dbmss 2.2-3

## Significant user-visible changes

- Updated CITATION: the paper about this package has been published: Eric Marcon, Stephane Traissac, Florence Puech, Gabriel Lang (2015). Tools to Characterize Point Patterns: dbmss for R. Journal of Statistical Software, 67(3), 1-15.



# dbmss 2.2-2

## Minor change

- Intervals of estimation of the Kd function are narrower when the distance range is small with respect to the window to improve precision.

# Bug fix

- Distances pairs more than twice the maximum value of `r` where ignored when using the approximate estimation of `Kd`, so `Kd` was overestimated when `r` was smaller than usual (much less than the default values). Corrected.



# dbmss 2.2-1

## Bug fixes

- Adjust argument ignored in `Kdhat`. Fixed.



# dbmss 2.2-0

## Significant user-visible changes
- `m` function added.
- Default value `Controls=NULL` for `Dhat`: controls are by default all points except for cases.
- `Kd` and `m` are both computed by default up to one third of the diameter of the window, other options are added.
- `Original` and `Adjust` arguments allow to change the default bandwith used by `Kd` and `m`.

## Minor change

- Typo in internal function name `FillEnveloppe` corrected: `FillEnvelope` is the new name.


  
# dbmss 2.1-2

## Significant user-visible changes

- Default neighbor type for `Khat` and `Kenvelope` is the same as reference type instead of "".
- `Kd` estimation now accepts "" as reference type to use all points.
- Approximate argument to calculate `Kd` on big data sets with little RAM.

## Bug fixes

- `FillEnvelope` returned wrong quantiles for local confidence intervals. Fixed.


  
# dbmss 2.1-1

## Significant user-visible changes

- Global confidence intervals are now defined even when `NA` values are found in the simulations.
- `Kd` is computed by default up to the median (instead of mean) distance between points.

## Bug fixes

- `Kdhat` with Reference and Neighbor points of different types was calculated with only half of point pairs and sometimes crashed R. Fixed.



# dbmss 2.1-0

## Significant user-visible changes

- `Kdhat` and `M` are computed through C++ loops instead of spatstat `pairdist()`. Much faster, and uses far less memory.


# dbmss 2.0-6

## Significant user-visible changes

- `Kdhat` and `M` propose a default value for argument `r`.
- Vignette added.

## Minor change

- `Kdhat` probability density estimation is cut at the lowest distance between points instead of 0.


# dbmss 2.0-5

## Bug fixes

- `Kdhat` with Reference and Neighbor points of the same type and `Weighted=TRUE` returned an error. Fixed.

  
# dbmss 2.0-3

## Overview

- Minor corrections, mainly comments and formating.

  
# dbmss 2.0-0

## Overview

- Most code rewritten.
  
## Significant user-visible changes

- Names of functions `X.r` became Xhat.
- Point patterns are now of class `wmppp`.
- Results are of class `fv`, envelopes of class `envelope`.

  
# dbmss 1.2-5

## Overview

- NEWS file added.


# dbmss 1.2-4

## Overview

- Documentation format improved.
 
## Significant user-visible changes

- The default behavior of `Kd` is that of Duranton and Overman (2005). An optional parameter is added to used improved bandwith selection.
 
## Bug fixes

- The density estimation of `Kd` was run twice. Fixed.



# dbmss 1.2-3

  
## ## Overview

- First version on CRAN. Versions 1.2.2 and 1.2.3 contain faster examples to follow CRAN requirements.

 

# dbmss 1.2-1
  
## Overview

- `Kinhom` function added.



# dbmss 1.1
  
## Overview

- First version.
