# Converts data to class Dtable

Creates an object of class `"Dtable"` representing a set of points with
weights and labels and the distances between them.. This is a generic
method.

## Usage

``` r
as.Dtable(X, ...)
  # S3 method for class 'ppp'
as.Dtable(X, ...)
  # S3 method for class 'data.frame'
as.Dtable(X, ...)
```

## Arguments

- X:

  Data to be converted into a `"Dtable"`.

- ...:

  Extra arguments.

## Details

This is a generic method, implemented for
[`ppp`](https://rdrr.io/pkg/spatstat.geom/man/ppp.html) and
[`data.frame`](https://rdrr.io/r/base/data.frame.html).

Data is first converted to a
([`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)).
Then, the distance matrix between points is calculated and the marks are
kept.

## Value

An object of class `"Dtable"`.

## See also

[`as.wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md)
