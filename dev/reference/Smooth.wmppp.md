# Spatial smoothing of individual dbmss's

Performs spatial smoothing of the individual values of distance-based
measures computed in the neighborhood of each point (Marcon and Puech,
2023).

## Usage

``` r
# S3 method for class 'wmppp'
Smooth (X, fvind, distance = NULL, Quantiles = FALSE, 
      sigma = bw.scott(X, isotropic = TRUE), Weighted = TRUE, Adjust = 1, 
      Nbx = 128, Nby = 128, ..., CheckArguments = TRUE)
```

## Arguments

- X:

  A point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

- fvind:

  An object of class `fv`, see
  [`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
  obtained a distance-based method, such as
  [`Mhat`](https://ericmarcon.github.io/dbmss/dev/reference/Mhat.md)
  with individual values (argument `Individual = TRUE`).

- distance:

  The distance at which the function value must be considered. The
  default value is the median distance used to calculate the function
  values.

- Quantiles:

  If `FALSE` (default), the dbmss is smoothed to produce a map of the
  measure. If `TRUE`, its quantiles (computed by
  [`Mhat`](https://ericmarcon.github.io/dbmss/dev/reference/Mhat.md)
  with argument `Quantiles = TRUE`) are smoothed to produce a map of the
  confidence level of the measure.

- Weighted:

  If `TRUE` (default), the point weights are taken into account for
  smoothing.

- sigma:

  The bandwidth used for smoothing. A Gaussian kernel is used (see
  [`Smooth.ppp`](https://rdrr.io/pkg/spatstat.explore/man/Smooth.ppp.html)).
  Its bandwidth is chosen by default according to Scott's rule (see
  [`bw.scott`](https://rdrr.io/pkg/spatstat.explore/man/bw.scott.html)).

- Adjust:

  Force the selected bandwidth (`sigma`) to be multiplied by `Adjust`.
  Setting it to values smaller than one (1/2 for example) will sharpen
  the estimation.

- Nbx, Nby:

  The number of columns and rows (pixels) of the resulting map, 128 by
  default. Increase it for quality, paid by increasing computing time.

- ...:

  Extra arguments, passed to
  [`Smooth.ppp`](https://rdrr.io/pkg/spatstat.explore/man/Smooth.ppp.html).

- CheckArguments:

  If `TRUE` (default), the function arguments are verified. Should be
  set to `FALSE` to save time in simulations for example, when the
  arguments have been checked elsewhere.

## Value

An image that can be plotted. If quantiles have been computed in
`fvind`, attributes "High" and "Low" contain logical vectors to
indentify significantly high and low quantiles.

## References

Marcon, E. and Puech, F. (2023). Mapping distributions in
non-homogeneous space with distance-based methods. *Journal of Spatial
Econometrics* 4(1), 13.

## Examples

``` r
  ReferenceType <- "V. Americana"
  NeighborType <- "Q. Rosea"
  # Calculate individual intertype M(distance) values
  fvind <- Mhat(paracou16, r=c(0, 30), ReferenceType, NeighborType, Individual=TRUE)
  # Plot the point pattern with values of M(30 meters)
  p16_map <- Smooth(paracou16, fvind, distance=30)
  plot(p16_map, main = "")
  # Add the reference points to the plot
  is.ReferenceType <- marks(paracou16)$PointType == ReferenceType
  points(x=paracou16$x[is.ReferenceType], y=paracou16$y[is.ReferenceType], pch=20)
  # Add contour lines
  contour(p16_map, nlevels = 5, add = TRUE)
```
