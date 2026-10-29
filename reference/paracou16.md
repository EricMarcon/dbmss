# Paracou field station plot 16, partial map

This point pattern is from Paracou field station, French Guiana, managed
by [Cirad](https://www.cirad.fr).

## Usage

``` r
data(paracou16)
```

## Format

An object of class
[`ppp.object`](https://rdrr.io/pkg/spatstat.geom/man/ppp.object.html)
representing the point pattern of tree locations in a 250 x 300 meter
sampling region. Each tree is marked with its species ("Q. Rosea", "V.
Americana" or "Other"), and basal area (square centimeters).

## Source

Permanent data census of Paracou and Marcon et al. (2012).

## References

Gourlet-Fleury, S., Guehl, J. M. and Laroussinie, O., Eds. (2004).
*Ecology & management of a neotropical rainforest. Lessons drawn from
Paracou, a long-term experimental research site in French Guiana*.
Paris, Elsevier.

Marcon, E., F. Puech and S. Traissac (2012). Characterizing the relative
spatial structure of point patterns. *International Journal of Ecology*
2012(Article ID 619281): 11.

## Examples

``` r
data(paracou16)
# Plot (second column of marks is Point Types) 
autoplot(paracou16, which.marks=2, leg.side="right")

```
