# Simulations of a point pattern according to the null hypothesis of random position defined for K

Simulations of a point pattern according to the null hypothesis of
random position defined for *K*.

## Usage

``` r
rRandomPositionK(X, Precision = 0, CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

- Precision:

  Accuracy of point coordinates, measured as a part of distance unit.
  See notes. Default is 0 for no approximation.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

Points marks are kept unchanged and their position is drawn in a
binomial process by
[`runifpoint`](https://rdrr.io/pkg/spatstat.random/man/runifpoint.html).

## Value

A new weighted, marked, planar point pattern (an object of class
`wmppp`, see
[`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

## Note

Simulations in a binomial process keeps the same number of points, so
that marks can be redistributed. If a real CSR simulation is needed and
marks are useless, use
[`rpoispp`](https://rdrr.io/pkg/spatstat.random/man/rpoispp.html).

Actual data coordinates are often rounded. Use the `Precision` argument
to simulate point patterns with the same rounding procedure. For
example, if point coordinates are in meters and rounded to the nearest
half meter, use `Precision = 0.5` so that the same approximation is
applied to the simulated point patterns.

## See also

[`rRandomLocation`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomLocation.md)

## Examples

``` r
# Simulate a point pattern with two types
X <- rpoispp(5) 
PointType   <- sample(c("A", "B"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
marks(X) <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

autoplot(X, main="Original pattern")


# Randomize it
Y <- rRandomPositionK(X)
# Points are randomly distributed
autoplot(Y, main="Randomized pattern")
```
