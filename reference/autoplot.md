# ggplot methods to plot dbmss objects

S3 methods for the
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
generic.

## Usage

``` r
# S3 method for class 'envelope'
autoplot(object, fmla, ..., ObsColor = "black",
        H0Color = "red", ShadeColor = "grey75", alpha = 0.3, main = NULL,
        xlab = NULL, ylab = NULL, LegendLabels = NULL)
  # S3 method for class 'fv'
autoplot(object, fmla, ..., ObsColor = "black",
        H0Color = "red", ShadeColor = "grey75", alpha = 0.3, main = NULL,
        xlab = NULL, ylab = NULL, LegendLabels = NULL)
  # S3 method for class 'wmppp'
autoplot(object, ..., show.window = TRUE,
        MaxPointTypes = 6, Other = "Other",
        main = NULL, xlab = NULL, ylab = NULL, LegendLabels = NULL,
        labelSize = "Weight", labelColor = "Type", palette="Set1",
        windowColor = "black", windowFill = "transparent", alpha = 1)
```

## Arguments

- object:

  An object to be plotted.

- fmla:

  An R language formula determining which variables or expressions are
  plotted. Either a formula object, or a string that can be parsed as a
  formula. See
  [`plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

- ...:

  Extra arguments, currently unused.

- ObsColor:

  The color of the line representing observed values of the function.

- H0Color:

  The color of the line representing the null hypothesis values of the
  function.

- ShadeColor:

  The color of the confidence envelope.

- alpha:

  The opacity of the confidence envelope (in function values) or the
  points (in maps), between 0 and 1.

- main:

  The title of the plot.

- xlab, ylab:

  The axes labels.

- LegendLabels:

  A vector of characters. The first two items describe the observed and
  null-hypothesis curves, the third and last item the confidence
  interval. To be used only in plots with two curves (typically observed
  and expected values). The default is \`NULL\` to display the full
  description of functions.

- show.window:

  if \`TRUE\`, the borders of the window containing the points are shown
  on the point map.

- MaxPointTypes:

  The maximum number of different point types to show. If the point set
  contains more of them, the less frequent ones are gathered as "Other".
  This number must be limited for readability and not to exceed the
  number of colors offered by the palette.

- Other:

  The name of the point types gathered as "Other".

- labelSize:

  The guide of the point size legend in point maps, i.e. what the
  \`PointSize\` mark represents.

- labelColor:

  The guide of the point color legend in point maps, i.e. what the
  \`PointType\` mark represents.

- palette:

  The color palette used to display point types in maps. See
  [`scale_color_brewer`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)

- windowColor:

  The color used to draw the limits of the windows in point maps.

- windowFill:

  The color used to fill the windows in point maps.

## Details

Plots of \`wmppp\` objects are a single representation of both point
types and point weights. Rectangular and polygonal windows (see
[`owin.object`](https://rdrr.io/pkg/spatstat.geom/man/owin.object.html))
are supported but mask windows are ignored (use the \`plot\` method if
necessary).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Eric Marcon \<Eric.Marcon@agroparistech.fr\>, parts of the code from
[`spatstat.explore::plot.fv`](https://rdrr.io/pkg/spatstat.explore/man/plot.fv.html).

## Examples

``` r
data(paracou16)
# Keep only 20% of points to run this example
X <- as.wmppp(rthin(paracou16, 0.2))
autoplot(X)


# Plot the envelope (should be 1000 simulations, reduced to 20 to save time)
autoplot(KdEnvelope(X, ReferenceType="Q. Rosea", NumberOfSimulations=20))


# With a formula and a compact legend
autoplot(KEnvelope(X, NumberOfSimulations=20),
    ./(pi*r^2) ~ r,
    LegendLabels=c("Observed", "Expected", "Confidence\n enveloppe"))
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_ribbon()`).
```
