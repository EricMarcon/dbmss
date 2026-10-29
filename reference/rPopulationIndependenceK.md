# Simulations of a point pattern according to the null hypothesis of population independence defined for K

Simulates of a point pattern according to the null hypothesis of
population independence defined for *K*.

## Usage

``` r
rPopulationIndependenceK(X, ReferenceType, NeighborType, CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)).

- ReferenceType:

  One of the point types.

- NeighborType:

  One of the point types.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

Reference points are kept unchanged, neighbor type point positions are
shifted by
[`rshift`](https://rdrr.io/pkg/spatstat.random/man/rshift.html). Other
points are lost and point weights are not kept (they are set to 1) since
the K function ignores them.

## Value

A new weighted, marked, planar point pattern (an object of class
`wmppp`, see
[`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)).

## References

Goreaud, F. et Pelissier, R. (2003). Avoiding misinterpretation of
biotic interactions with the intertype K12 fonction: population
independence vs random labelling hypotheses. *Journal of Vegetation
Science* 14(5): 681-692.

## See also

[`rPopulationIndependenceM`](https://ericmarcon.github.io/dbmss/reference/rPopulationIndependenceM.md),
[`rRandomLabeling`](https://ericmarcon.github.io/dbmss/reference/rRandomLabeling.md)

## Examples

``` r
# Simulate a point pattern with three types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
marks(X) <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

# Plot the point pattern, using PointType as marks
autoplot(X, main="Original pattern")


# Randomize it
Y <- rPopulationIndependenceK(X, "A", "B")
# Points of type "A" are unchanged, points of type "B" have been moved altogether
# Other points are lost and point weights are set to 1
autoplot(Y, main="Randomized pattern")
```
