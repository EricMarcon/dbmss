# Test of a point pattern against Complete Spatial Randomness

Tests the point pattern against CSR using values of the *K* function

## Usage

``` r
Ktest(X, r)
```

## Arguments

- X:

  A point pattern
  ([`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)).
  Marks are ignored. The window must be a rectangle sensu spatstat
  (tested by
  [`is.rectangle`](https://rdrr.io/pkg/spatstat.geom/man/is.rectangle.html)).

- r:

  A vector of distances.

## Details

The test returns the risk to reject CSR erroneously, i.e. the p-value of
the test, based on the distribution of the *K* function.

If `r` includes 0, it will be silently removed because no neighbor point
can be found at distance 0. The longer `r`, the more accurate the test
is in theory but at the cost of computation time first, and of
computation accuracy then because a matrix of size the length of `r`
must be inverted. 10 values in `r` seems to be a reasonable choice.

## Value

A p-value.

## References

Lang, G. and Marcon, E. (2013). Testing randomness of spatial point
patterns with the Ripley statistic. *ESAIM: Probability and Statistics.*
17: 767-788.

Marcon, E., S. Traissac, and Lang, G. (2013). A Statistical Test for
Ripley's Function Rejection of Poisson Null Hypothesis. *ISRN Ecology*
2013(Article ID 753475): 9.

## Author

Gabriel Lang \<Gabriel.Lang@agroparistech.fr\>, Eric
Marcon\<Eric.Marcon@agroparistech.fr\>

## See also

[`Khat`](https://ericmarcon.github.io/dbmss/dev/reference/Khat.md),
[`GoFtest`](https://ericmarcon.github.io/dbmss/dev/reference/GoFtest.md)

## Examples

``` r
# Simulate a Matern (Neyman Scott) point pattern
nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre=c(x0, y0)))
}
X <- rNeymanScott(20, 0.1, nclust, radius=0.2, n=5)
autoplot(as.wmppp(X))


# Test it
Ktest(X, r=seq(0.1, .5, .1))
#> [1] 0.02448668
```
