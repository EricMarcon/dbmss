# Class of Weighted, Marked, Planar Point Patterns

A class `"wmppp"` to represent a two-dimensional point pattern of class
[`ppp`](https://rdrr.io/pkg/spatstat.geom/man/ppp.html) whose marks are
a dataframe with two columns:

- `PointType`: labels, as factors

- `PointWeight`: weights.

## Details

This class represents a two-dimensional point pattern dataset. `wmppp`
objects are also of class
[`ppp`](https://rdrr.io/pkg/spatstat.geom/man/ppp.html).

Objects of class `wmppp` may be created by the function
[`wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.md) and
converted from other types of data by the function
[`as.wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md).

## See also

[`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html),
[`wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.md),
[`as.wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md)
[`autoplot.wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/autoplot.md)

## Examples

``` r
# Draw the coordinates of 10 points
X <- runif(10)
Y <- runif(10)
# Draw the point types and weights
PointType   <- sample(c("A", "B"), 10, replace=TRUE)
PointWeight <- runif(10)
# Build the point pattern
X <- wmppp(data.frame(X, Y, PointType, PointWeight), owin())

# Plot the point pattern. which.marks=1 for point weights, 2 for point types
par(mfrow=c(1,2))
plot(X, which.marks=1, main="Point weights")
plot(X, which.marks=2, main="Point types")


# Or use autoplot for a ggplot
autoplot(X)
```
