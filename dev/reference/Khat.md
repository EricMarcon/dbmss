# Estimation of the K function

Estimates the *K* function

## Usage

``` r
Khat(X, r = NULL, ReferenceType = "", NeighborType = ReferenceType, CheckArguments = TRUE)
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

- NeighborType:

  One of the point types. By default, the same as reference type.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

*K* is a cumulative, topographic measure of a point pattern structure.

## Value

An object of class `fv`, see
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
which can be plotted directly using
[`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## References

Ripley, B. D. (1976). The Foundations of Stochastic Geometry. *Annals of
Probability* 4(6): 995-998.

Ripley, B. D. (1977). Modelling Spatial Patterns. *Journal of the Royal
Statistical Society B* 39(2): 172-212.

## Note

The computation of `Khat` relies on spatstat functions
[`Kest`](https://rdrr.io/pkg/spatstat.explore/man/Kest.html) and
[`Kcross`](https://rdrr.io/pkg/spatstat.explore/man/Kcross.html).

## See also

[`Lhat`](https://ericmarcon.github.io/dbmss/dev/reference/Lhat.md),
[`KEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/KEnvelope.md),
[`Ktest`](https://ericmarcon.github.io/dbmss/dev/reference/Ktest.md)

## Examples

``` r
data(paracou16)
autoplot(paracou16)


# Calculate K
r <- 0:30
(Paracou <- Khat(paracou16, r))
#> Function value object (class ‘fv’)
#> for the function r -> K(r)
#> ................................................................
#>      Math.label     Description                                 
#> r    r              distance argument r                         
#> theo K[pois](r)     theoretical Poisson K(r)                    
#> iso  hat(K)[iso](r) Ripley isotropic correction estimate of K(r)
#> ................................................................
#> Default plot formula:  .~r
#> where “.” stands for ‘iso’, ‘theo’
#> Recommended range of argument r: [0, 30]
#> Available range of argument r: [0, 30]
#> Unit of length: 1 meter

# Plot (after normalization by pi.r^2)
autoplot(Paracou, ./(pi*r^2) ~ r)
```
