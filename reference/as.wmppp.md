# Converts data to class wmppp

Creates a Weighted, Marked, Planar Point Pattern, *i.e.* an object of
class `"wmppp"` representing a two-dimensional point pattern with
weights and labels. This is a generic method.

## Usage

``` r
as.wmppp(X, ...)
  # S3 method for class 'ppp'
as.wmppp(X, ...)
  # S3 method for class 'data.frame'
as.wmppp(X, window = NULL, unitname = NULL, ...)
```

## Arguments

- X:

  Data to be converted into a weighted, marked, planar point pattern
  ([`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md))

- window:

  An object of calls "owin"
  ([`owin.object`](https://rdrr.io/pkg/spatstat.geom/man/owin.object.html)).

- unitname:

  Name of unit of length. Either a single character string, or a vector
  of two character strings giving the singular and plural forms,
  respectively.

- ...:

  Extra arguments.

## Details

This is a generic method, implemented for
[`ppp`](https://rdrr.io/pkg/spatstat.geom/man/ppp.html) and
[`data.frame`](https://rdrr.io/r/base/data.frame.html):

- If the dataset `X` is an object of class "ppp"
  ([`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)),
  the marks are converted to point weights if they are numeric or to
  point types if they are factors. Default weights are set to 1, default
  types to "All". If marks are a dataframe with column names equal to
  `PointType` and `PointWeight`, they are not modified. Row names of the
  dataframe are preserved as row names of the marks, to identify points.

- If the dataset `X` is a dataframe, see
  [`wmppp`](https://ericmarcon.github.io/dbmss/reference/wmppp.md).

## Value

An object of class `"wmppp"`.

## See also

[`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)
