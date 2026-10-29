# Transform simulation values to an fv

This function is used internally to calculate envelope values and store
them into an
[`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html).

## Usage

``` r
FillEnvelope(Envelope, Alpha, Global)
```

## Arguments

- Envelope:

  An envelope object
  ([`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html))
  containing all the simulated function values.

- Alpha:

  The risk level.

- Global:

  Logical; if `TRUE`, a global envelope sensu Duranton and
  Overman (2005) is calculated.

## Value

Returns the envelope object
([`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html))
with `hi` and `lo` values calculated from the simlations.
