# Estimation of the g function

Estimates the *g* function

## Usage

``` r
ghat(X, r = NULL, ReferenceType = "", NeighborType = "", CheckArguments = TRUE)
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

  One of the point types. Default is all point types.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

The computation of `ghat` relies on spatstat function
[`sewpcf`](https://rdrr.io/pkg/spatstat.explore/man/spatstat.explore-internal.html).
The kernel estimation of the number of neighbors follows Stoyan and
Stoyan (1994, pages 284–285).

## Value

An object of class `fv`, see
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
which can be plotted directly using
[`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## References

Stoyan, D. and Stoyan, H. (1994) *Fractals, random shapes and point
fields: methods of geometrical statistics*. John Wiley and Sons.

## See also

[`gEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/gEnvelope.md)

## Examples

``` r
data(paracou16)
autoplot(paracou16)


# Calculate g
r <- 0:30
(Paracou <- ghat(paracou16, r, "Q. Rosea", "V. Americana"))
#> Function value object (class ‘fv’)
#> for the function r -> g(r)
#> ................................................................
#>      Math.label     Description                                 
#> r    r              distance argument r                         
#> theo g[pois](r)     theoretical Poisson g(r)                    
#> iso  hat(g)[iso](r) Ripley isotropic correction estimate of g(r)
#> ................................................................
#> Default plot formula:  .~r
#> where “.” stands for ‘theo’, ‘iso’
#> Recommended range of argument r: [0, 30]
#> Available range of argument r: [0, 30]
#> Unit of length: 1 meter

# Plot
autoplot(Paracou)
```
