# Estimation of the Lmm function

Estimates the *Lmm* function

## Usage

``` r
Lmmhat(X, r = NULL, ReferenceType = "", CheckArguments = TRUE)
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

  One of the point types. Others are ignored. Default is all point
  types.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

*Lmm* is the normalized version of *Kmm*:
\\Lmm(r)=\sqrt{\frac{Kmm}{\pi}}-r\\.

## Value

An object of class `fv`, see
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
which can be plotted directly using
[`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## References

Penttinen, A., Stoyan, D. and Henttonen, H. M. (1992). Marked Point
Processes in Forest Statistics. *Forest Science* 38(4): 806-824.

Espa, G., Giuliani, D. and Arbia, G. (2010). Weighting Ripley's
K-function to account for the firm dimension in the analysis of spatial
concentration. *Discussion Papers*, 12/2010. Universita di Trento,
Trento: 26.

## See also

[`Kmmhat`](https://ericmarcon.github.io/dbmss/dev/reference/Kmmhat.md),
[`LmmEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/LmmEnvelope.md)

## Examples

``` r
data(paracou16)
# Keep only 50% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.5))
autoplot(X, 
  labelSize = expression("Basal area (" ~cm^2~ ")"), 
  labelColor = "Species")


# Calculate Lmm
r <- seq(0, 30, 2)
(Paracou <- Lmmhat(X, r))
#> Function value object (class ‘fv’)
#> for the function r -> L[mm](r)
#> .............................................................................
#>      Math.label              Description                                     
#> r    r                       distance argument r                             
#> theo {L[mm][]^{pois}}(r)     theoretical Poisson L[mm](r)                    
#> iso  {hat(L[mm])[]^{iso}}(r) Ripley isotropic correction estimate of L[mm](r)
#> .............................................................................
#> Default plot formula:  .~.x
#> where “.” stands for ‘iso’, ‘theo’
#> Recommended range of argument r: [0, 50]
#> Available range of argument r: [0, 50]
#> Unit of length: 1 meter

# Plot
autoplot(Paracou)
```
