# Auxiliary functions to count point neighbors

C++ routines used for fast count of neighbors.

## Usage

``` r
parallelCountNbd(r, x, y, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdCC(r, x, y, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdm(x, y, ReferencePoints)
parallelCountNbdDt(r, Dmatrix, Weight, IsReferenceType, IsNeighborType)
parallelCountNbdDtCC(r, Dmatrix, Weight, IsReferenceType, IsNeighborType)
DistKd(x, y, PointWeight, Weight, Dist, IsReferenceType, IsNeighborType)
CountNbdKd(r, x, y, Weight, Nbd, IsReferenceType, IsNeighborType)
```

## Arguments

- r:

  A vector of distances.

- x:

  A vector of *x* coordinates.

- y:

  A vector of *y* coordinates.

- Dmatrix:

  A matrix of distances.

- PointWeight:

  A vector of point weights.

- Weight:

  A vector of point weights.

- ReferencePoints:

  A vector of pont indices.

- Nbd:

  A matrix to store the weight of neighbors.

- Dist:

  A vector of point-pair distances.

- IsReferenceType:

  A boolean vector to recognize reference points.

- IsNeighborType:

  A boolean vector to recognize neighbor points.

## Details

These routines are called internally.
