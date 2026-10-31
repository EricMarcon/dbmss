# Estimation of the global confidence interval of simulations

Calculates the global confidence interval envelope sensu Duranton and
Overman (2005) according to simulations of the null hypothesis of a
function.

## Usage

``` r
GlobalEnvelope(Simulations, Alpha)
```

## Arguments

- Simulations:

  An object of class `fv`, see
  [`fv.object`](https://rdrr.io/pkg/spatstat.explore/man/fv.object.html),
  containing the simulated values.

- Alpha:

  The risk level.

## Details

The global envelope is calculated by iteration: the simulations reaching
one of the upper or lower values at any distance are eliminated at each
step. The process is repeated until *Alpha / Number of simulations*
simulations are dropped. The remaining upper and lower bounds at all
distances constitute the global envelope. Interpolation is used if the
exact ratio cannot be reached.

## Value

A matrix with two lines:

- Glo:

  the lower bound of the envelope

- Ghi:

  the upper bound of the envelope

## References

Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using
Micro-Geographic Data. *Review of Economic Studies* 72(4): 1077-1106.
