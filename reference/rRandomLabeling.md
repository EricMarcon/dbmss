# Simulations of a point pattern according to the null hypothesis of random labeling

Simulates of a point pattern according to the null hypothesis of random
labeling.

## Usage

``` r
rRandomLabeling(X, CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)).

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

Marks are redistributed randomly across the original point pattern.

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

[`rRandomLabelingM`](https://ericmarcon.github.io/dbmss/reference/rRandomLabelingM.md),
[`rPopulationIndependenceK`](https://ericmarcon.github.io/dbmss/reference/rPopulationIndependenceK.md)

## Examples

``` r
# Simulate a point pattern with five types
X <- rpoispp(50) 
PointType   <- sample(c("A", "B", "C", "D", "E"), X$n, replace=TRUE)
PointWeight <- runif(X$n, min=1, max=10)
marks(X) <- data.frame(PointType, PointWeight)
X <- as.wmppp(X)

autoplot(X, main="Original pattern")


# Randomize it
Y <- rRandomLabeling(X)
# Types and weights have been redistributed randomly across locations
autoplot(Y, main="Randomized pattern")
```
