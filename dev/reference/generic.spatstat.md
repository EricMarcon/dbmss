# Methods for weighted, marked planar point patterns (of class wmppp) from spatstat

**spatstat** methods for a
[`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)
applied to a
[`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md).

## Usage

``` r
# S3 method for class 'wmppp'
sharpen(X, ...)
# S3 method for class 'wmppp'
superimpose(...)
# S3 method for class 'wmppp'
unique(x, ...)
# S3 method for class 'wmppp'
i[j, drop = FALSE, ..., clip = FALSE]
```

## Arguments

- X, x:

  A two-dimensional point pattern. An object of class "wmppp".

- ...:

  Arguments passed to the
  [`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)
  method.

- i:

  Subset index. Either a valid subset index in the usual R sense,
  indicating which points should be retained, or a window (an object of
  class "owin") delineating a subset of the original observation window,
  or a pixel image with logical values defining a subset of the original
  observation window.

- j:

  Redundant. Included for backward compatibility.

- drop:

  Logical value indicating whether to remove unused levels of the marks,
  if the marks are a factor.

- clip:

  Logical value indicating how to form the window of the resulting point
  pattern, when `i` is a window. If `clip=FALSE` (the default), the
  result has window equal to `i`. If `clip=TRUE`, the resulting window
  is the intersection between the window of `x` and the window `i`.

## Details

**spatstat** methods for `ppp` objects returning a `ppp` object can be
applied to a `wmppp` and return a `wpppp` with these methods which just
call the
[`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)
method and change the class of the result for convenience.

Some **spatstat** functions such as
[`rthin`](https://rdrr.io/pkg/spatstat.random/man/rthin.html) are not
generic so they always return a
[`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)
when applied to a
[`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md).
Their result may be converted by
[`as.wmppp`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md).

## Value

An object of class `"wmppp"`.

## See also

[`sharpen.ppp`](https://rdrr.io/pkg/spatstat.explore/man/sharpen.html),
[`superimpose.ppp`](https://rdrr.io/pkg/spatstat.geom/man/superimpose.html),
[`unique.ppp`](https://rdrr.io/pkg/spatstat.geom/man/unique.ppp.html)
