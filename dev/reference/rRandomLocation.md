# Simulations of a point pattern according to the null hypothesis of random location

Simulates of a point pattern according to the null hypothesis of random
location.

## Usage

``` r
rRandomLocation(X, ReferenceType = "", CheckArguments = TRUE)
```

## Arguments

- X:

  A weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

- ReferenceType:

  One of the point types.

- CheckArguments:

  Logical; if `TRUE`, the function arguments are verified. Should be set
  to `FALSE` to save time in simulations for example, when the arguments
  have been checked elsewhere.

## Details

Points are redistributed randomly across the locations of the original
point pattern. This randomization is equivalent to random labeling,
considering the label is both point type and point weight. If
`ReferenceType` is specified, then only reference type points are kept
in the orginal point pattern before randomization.

## Value

A new weighted, marked, planar point pattern (an object of class
`wmppp`, see
[`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).

## References

Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using
Micro-Geographic Data. *Review of Economic Studies* 72(4): 1077-1106.

Marcon, E. and Puech, F. (2010). Measures of the Geographic
Concentration of Industries: Improving Distance-Based Methods. *Journal
of Economic Geography* 10(5): 745-762.

## See also

[`rRandomPositionK`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomPositionK.md)

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
Y <- rRandomLocation(X)
# Points have been redistributed randomly across locations
autoplot(Y, main="Randomized pattern")
```
