# Checks the arguments of a function of the package dbmss

This function is used internally to verify that arguments passed to
dbmss functions such as
[`Mhat`](https://ericmarcon.github.io/dbmss/dev/reference/Mhat.md) are
correct.

## Usage

``` r
CheckdbmssArguments()
```

## Value

Returns `TRUE` or stops if a problem is detected.

## Details

The function compares the arguments passed to its parent function to the
type they should be and performs some extra tests (*e.g.* risk threshold
must be between 0 and 1). It stops if an argument is not correct.
