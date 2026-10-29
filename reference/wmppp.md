# Create a Weighted, Marked, Planar Point Pattern

Creates an object of class `"wmppp"` representing a two-dimensional
point pattern with weights and labels.

## Usage

``` r
wmppp(df, window = NULL, unitname = NULL)
```

## Arguments

- df:

  A dataframe with at least two columns containing point coordinates.

- window:

  An object of calls "owin"
  ([`owin.object`](https://rdrr.io/pkg/spatstat.geom/man/owin.object.html)).

- unitname:

  Name of unit of length. Either a single character string, or a vector
  of two character strings giving the singular and plural forms,
  respectively. Ignored if `window` is not `NULL`.

## Details

Columns named "X", "Y", "PointType", "PointWeight" (capitalization is
ignored) are searched to build the `"wmppp"` object and set the point
coordinates, type and weight. If they are not found, columns are used in
this order. If columns are missing, `PointType` is set to "All" and
`PointWeight` to 1. If a "PointName" column is found, it is used to set
the row names of the marks, else the original row names are used.

If the window is not specified, a rectangle containing all points is
used, and `unitname` is used.

## Value

An object of class `"wmppp"`.

## See also

[`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md),

## Examples

``` r
# Draw the coordinates of 10 points
X <- runif(10)
Y <- runif(10)
# Draw the point types.
PointType   <- sample(c("A", "B"), 10, replace=TRUE)
# Plot the point pattern. Weights are set to 1 ant the window is adjusted.
plot(wmppp(data.frame(X, Y, PointType)), , which.marks=2)
#> Warning: No column has been found for PointWeight. All point weights have been set to 1.
#> Warning: No window has been specified. A rectangle window containing all points has been used.
```
