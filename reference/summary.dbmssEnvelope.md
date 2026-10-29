# Summary of a confidence envelope

Prints a useful summary of a confidence envelope of class
"dbmssEnvelope"

## Usage

``` r
# S3 method for class 'dbmssEnvelope'
summary(object, ...)
```

## Arguments

- object:

  An object of class "dbmssEnvelope".

- ...:

  Ignored.

## Details

"dbmssEnvelope" objects are similar to
[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html)
objects. Their summary is different to take into account the possibility
of building global envelope following Duranton and Overman (2005): the
global envelope is calculated by iteration: the simulations reaching one
of the upper or lower values at any distance are eliminated at each
step. The process is repeated until *Alpha / Number of simulations*
simulations are dropped. The remaining upper and lower bounds at all
distances constitute the global envelope. Interpolation is used if the
exact ratio cannot be reached.

## References

Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using
Micro-Geographic Data. *Review of Economic Studies* 72(4): 1077-1106

## Examples

``` r
data(paracou16)
autoplot(paracou16)


# Calculate intertype K envelope
Envelope <- KEnvelope(paracou16, NumberOfSimulations = 20, Global = TRUE,
  ReferenceType = "V. Americana", NeighborType = "Q. Rosea")
autoplot(Envelope)

summary(Envelope)
#> Global critical envelopes obtained from 20 simulations of "K"["V..Americana", "Q..Rosea"](r) under the null hypothesis: Random Position 
#> (All 20 simulated function values are stored in attr(, “simfuns” ) )
#> Significance level of Monte Carlo test: 0.05 
#> Data: X 
```
