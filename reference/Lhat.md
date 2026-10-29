# Estimation of the L function

Estimates the *L* function

## Usage

``` r
Lhat(X, r = NULL, ReferenceType = "", NeighborType = "", CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)).

- r:

  A vector of distances. If `NULL`, a sensible default value is chosen
  (512 intervals, from 0 to half the diameter of the window) following
  **spatstat**.

- ReferenceType:

  One of the point types. Default is all point types.

- NeighborType:

  One of the point types. Default is all point types.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

*L* is the normalized version of *K*: \\L(r)=\sqrt{\frac{K}{\pi}}-r\\.

## Value

An object of class `fv`, see
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
which can be plotted directly using
[`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## References

Besag, J. E. (1977). Comments on Ripley's paper. *Journal of the Royal
Statistical Society B* 39(2): 193-195.

## Note

*L* was originally defined as \\L(r)=\sqrt{\frac{K}{\pi}}\\. It has been
used as \\L(r)=\sqrt{\frac{K}{\pi}}-r\\ in a part of the literature
because this normalization is easier to plot.

## See also

[`Khat`](https://ericmarcon.github.io/dbmss/reference/Khat.md),
[`LEnvelope`](https://ericmarcon.github.io/dbmss/reference/LEnvelope.md)

## Examples

``` r
data(paracou16)
autoplot(paracou16)


# Calculate L
r <- 0:30
(Paracou <- Lhat(paracou16, r))
#> Function value object (class ‘fv’)
#> for the function r -> L(r)
#> ................................................................
#>      Math.label     Description                                 
#> r    r              distance argument r                         
#> theo L[pois](r)     theoretical Poisson L(r)                    
#> iso  hat(L)[iso](r) Ripley isotropic correction estimate of L(r)
#> ................................................................
#> Default plot formula:  .~r
#> where “.” stands for ‘iso’, ‘theo’
#> Recommended range of argument r: [0, 30]
#> Available range of argument r: [0, 30]
#> Unit of length: 1 meter

# Plot
autoplot(Paracou)
```
