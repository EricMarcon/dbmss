# Create a Distance table object.

Creates an object of class `"Dtable"` representing a set of points with
weights and labels and the distances between them.

## Usage

``` r
Dtable(Dmatrix, PointType = NULL, PointWeight = NULL)
```

## Arguments

- Dmatrix:

  A square matrix containing distances.

- PointType:

  A vector describing the point types. Its length must correspond to the
  number of points.

- PointWeight:

  A vector describing the point weights. Its length must correspond to
  the number of points.

## Details

The distance matrix is not necessarily symmetric, so distances are
understood in the common sense, not in the mathematical sense.
Asymmetric distances are appropriate when paths between points are
one-way only.

The points of origin are in lines, the targets in columns. The diagonal
of the matrix must contain zeros (the distance between a point and
itself is 0), and all other distances must be positive (they can be 0).

## Value

An object of class `"Dtable"`. It is a list:

- Dmatrix:

  The distance matrix.

- n:

  The number of points.

- marks:

  A list of two items: `PointType`, a vector of factors containing the
  point types and `PointWeight`, the numeric vector of weights.

## See also

[`as.Dtable`](https://ericmarcon.github.io/dbmss/reference/as.Dtable.md)

## Examples

``` r
# A Dtable containing two points
Dmatrix <- matrix(c(0,1,1,0), nrow=2)
PointType <- c("Type1", "Type2")
PointWeight <- c(2,3)
Dtable(Dmatrix, PointType, PointWeight)
#> $Dmatrix
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1    0
#> 
#> $n
#> [1] 2
#> 
#> $marks
#> $marks$PointType
#> [1] Type1 Type2
#> Levels: Type1 Type2
#> 
#> $marks$PointWeight
#> [1] 2 3
#> 
#> 
#> attr(,"class")
#> [1] "Dtable"
```
