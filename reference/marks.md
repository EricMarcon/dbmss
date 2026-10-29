# marks method for Dtable and wmppp objects

S3 methods for the
[`marks`](https://rdrr.io/pkg/spatstat.geom/man/marks.html) generic.

## Usage

``` r
# S3 method for class 'Dtable'
marks(x, ...)
  # S3 method for class 'Dtable'
marks(x, ...) <- value
  # S3 method for class 'wmppp'
marks(x, ..., dfok = TRUE, drop = TRUE) <- value
```

## Arguments

- x:

  A [`Dtable`](https://ericmarcon.github.io/dbmss/reference/Dtable.md)
  or
  [`wmppp.object`](https://ericmarcon.github.io/dbmss/reference/wmppp.object.md)
  object.

- ...:

  Extra arguments, currently unused.

- value:

  The value to set.

- dfok:

  Ignored.

- drop:

  Ignored.

## Details

These functions extract or modify the marks of a
[`Dtable`](https://ericmarcon.github.io/dbmss/reference/Dtable.md).

\`marks\<-.wmppp()\` just calls \`marks\<-.ppp()\` and keeps the class
of the wmppp object. The conformity of the marks with the definition of
the class "wmppp", i.e. a dataframe with columns "PointType" and
"PointWeight" of the same length as the number of points, is not
checked.

## Value

A dataframe with columns "PointType" and "PointWeight".

## Author

Eric Marcon \<Eric.Marcon@agroparistech.fr\>

## Examples

``` r
# A Dtable containing two points
Dmatrix <- matrix(c(0,1,1,0), nrow=2)
PointType <- c("Type1", "Type2")
PointWeight <- c(2,3)
X <- Dtable(Dmatrix, PointType, PointWeight)
# Extract the marks
marks(X)
#>   PointType PointWeight
#> 1     Type1           2
#> 2     Type2           3
```
