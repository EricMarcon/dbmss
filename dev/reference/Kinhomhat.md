# Estimation of the inhomogenous K function

Estimates the *Kinhom* function

## Usage

``` r
Kinhomhat(X, r = NULL, ReferenceType = "", lambda = NULL, CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

- r:

  A vector of distances. If `NULL`, a sensible default value is chosen
  (512 intervals, from 0 to half the diameter of the window) following
  **spatstat**.

- ReferenceType:

  One of the point types. Default is all point types.

- lambda:

  An estimation of the point pattern density, obtained by the
  [`density.ppp`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html)
  function.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

*Kinhom* is a cumulative, topographic measure of an inhomogenous point
pattern structure.

By default, density estimation is performed at points by
[`density.ppp`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html)
using the optimal bandwith
([`bw.diggle`](https://rdrr.io/pkg/spatstat.explore/man/bw.diggle.html)).
It can be calculated separately (see example), including at pixels if
the point pattern is too large for the default estimation to succeed,
and provided as the argument `lambda`: Arbia et al. (2012) for example
use another point pattern as a reference to estimate density.

Bivariate *Kinhom* is not currently supported.

## Value

An object of class `fv`, see
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
which can be plotted directly using
[`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## References

Baddeley, A. J., J. Moller, et al. (2000). Non- and semi-parametric
estimation of interaction in inhomogeneous point patterns. *Statistica
Neerlandica* 54(3): 329-350.

Arbia, G., G. Espa, et al. (2012). Clusters of firms in an inhomogeneous
space: The high-tech industries in Milan. *Economic Modelling* 29(1):
3-11.

## Note

The computation of `Kinhomhat` relies on spatstat functions
[`Kinhom`](https://rdrr.io/pkg/spatstat.explore/man/Kinhom.html),
[`density.ppp`](https://rdrr.io/pkg/spatstat.explore/man/density.ppp.html)
and
[`bw.diggle`](https://rdrr.io/pkg/spatstat.explore/man/bw.diggle.html).

## See also

[`KinhomEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/KinhomEnvelope.md),
[`Kinhom`](https://rdrr.io/pkg/spatstat.explore/man/Kinhom.html)

## Examples

``` r
data(paracou16)

# Density of all trees
lambda <- density.ppp(paracou16, bw.diggle(paracou16))
plot(lambda)
# Reduce the point pattern to one type of trees
V.americana <- paracou16[marks(paracou16)$PointType=="V. Americana"]
plot(V.americana, add=TRUE)
#> Plotting the first column of marks


# Calculate Kinhom according to the density of all trees
r <- 0:30
autoplot(Kinhomhat(paracou16, r, "V. Americana", lambda), ./(pi*r^2) ~ r)
```
