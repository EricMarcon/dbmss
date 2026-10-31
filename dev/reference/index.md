# Package index

## Package summary

- [`dbmss-package`](https://ericmarcon.github.io/dbmss/dev/reference/dbmss-package.md)
  [`dbmss`](https://ericmarcon.github.io/dbmss/dev/reference/dbmss-package.md)
  : Distance Based Measures of Spatial Structures

## Datasets

Example datasets.

- [`paracou16`](https://ericmarcon.github.io/dbmss/dev/reference/paracou16.md)
  : Paracou field station plot 16, partial map

## Prepare data

Data conversion and random generation.

- [`as.Dtable()`](https://ericmarcon.github.io/dbmss/dev/reference/as.Dtable.md)
  : Converts data to class Dtable
- [`as.wmppp()`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md)
  : Converts data to class wmppp
- [`wmppp()`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.md)
  : Create a Weighted, Marked, Planar Point Pattern
- [`wmppp.object`](https://ericmarcon.github.io/dbmss/dev/reference/wmppp.object.md)
  : Class of Weighted, Marked, Planar Point Patterns
- [`Dtable()`](https://ericmarcon.github.io/dbmss/dev/reference/Dtable.md)
  : Create a Distance table object.
- [`rPopulationIndependenceK()`](https://ericmarcon.github.io/dbmss/dev/reference/rPopulationIndependenceK.md)
  : Simulations of a point pattern according to the null hypothesis of
  population independence defined for K
- [`rPopulationIndependenceM()`](https://ericmarcon.github.io/dbmss/dev/reference/rPopulationIndependenceM.md)
  : Simulations of a point pattern according to the null hypothesis of
  population independence defined for M
- [`rRandomLabeling()`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomLabeling.md)
  : Simulations of a point pattern according to the null hypothesis of
  random labeling
- [`rRandomLabelingM()`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomLabelingM.md)
  : Simulations of a point pattern according to the null hypothesis of
  random labelling defined for M
- [`rRandomLocation()`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomLocation.md)
  : Simulations of a point pattern according to the null hypothesis of
  random location
- [`rRandomPositionK()`](https://ericmarcon.github.io/dbmss/dev/reference/rRandomPositionK.md)
  : Simulations of a point pattern according to the null hypothesis of
  random position defined for K

## Distance-based functions

Non-parametric summary statistics of point patterns.

- [`Dhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Dhat.md) :
  Estimation of the D function
- [`Kdhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Kdhat.md)
  : Estimation of the Kd function
- [`Khat()`](https://ericmarcon.github.io/dbmss/dev/reference/Khat.md) :
  Estimation of the K function
- [`Kinhomhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Kinhomhat.md)
  : Estimation of the inhomogenous K function
- [`Kmmhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Kmmhat.md)
  : Estimation of the Kmm function
- [`Lhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Lhat.md) :
  Estimation of the L function
- [`Lmmhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Lmmhat.md)
  : Estimation of the Lmm function
- [`Mhat()`](https://ericmarcon.github.io/dbmss/dev/reference/Mhat.md) :
  Estimation of the M function
- [`ghat()`](https://ericmarcon.github.io/dbmss/dev/reference/ghat.md) :
  Estimation of the g function
- [`mhat()`](https://ericmarcon.github.io/dbmss/dev/reference/m_hat.md)
  : Estimation of the m function

## Confidence envelopes

Confidence envelopes for the non-parametric summary statistics of point
patterns.

- [`DEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/DEnvelope.md)
  : Estimation of the confidence envelope of the D function under its
  null hypothesis
- [`KEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/KEnvelope.md)
  : Estimation of the confidence envelope of the K function under its
  null hypothesis
- [`KdEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/KdEnvelope.md)
  : Estimation of the confidence envelope of the Kd function under its
  null hypothesis
- [`KinhomEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/KinhomEnvelope.md)
  : Estimation of the confidence envelope of the Kinhom function under
  its null hypothesis
- [`KmmEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/KmmEnvelope.md)
  : Estimation of the confidence envelope of the Lmm function under its
  null hypothesis
- [`LEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/LEnvelope.md)
  : Estimation of the confidence envelope of the L function under its
  null hypothesis
- [`LmmEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/LmmEnvelope.md)
  : Estimation of the confidence envelope of the Lmm function under its
  null hypothesis
- [`MEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/MEnvelope.md)
  : Estimation of the confidence envelope of the M function under its
  null hypothesis
- [`gEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/gEnvelope.md)
  : Estimation of the confidence envelope of the g function under its
  null hypothesis
- [`mEnvelope()`](https://ericmarcon.github.io/dbmss/dev/reference/m_Envelope.md)
  : Estimation of the confidence envelope of the m function under its
  null hypothesis
- [`print(`*`<dbmssEnvelope>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/print.dbmssEnvelope.md)
  : Print a confidence envelope
- [`summary(`*`<dbmssEnvelope>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/summary.dbmssEnvelope.md)
  : Summary of a confidence envelope

## Tests

Statistical tests.

- [`GoFtest()`](https://ericmarcon.github.io/dbmss/dev/reference/GoFtest.md)
  : Goodness of Fit test between a distance based measure of spatial
  structure and simulations of its null hypothesis
- [`Ktest()`](https://ericmarcon.github.io/dbmss/dev/reference/Ktest.md)
  : Test of a point pattern against Complete Spatial Randomness

## Utilities

- [`dbmssEnvelope.object`](https://ericmarcon.github.io/dbmss/dev/reference/dbmssEnvelope.md)
  : Class of envelope of function values (fv)
- [`autoplot(`*`<envelope>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/autoplot.md)
  [`autoplot(`*`<fv>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/autoplot.md)
  [`autoplot(`*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/autoplot.md)
  : ggplot methods to plot dbmss objects
- [`envelope(`*`<Dtable>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/envelope.Dtable.md)
  : Computes simulation envelopes of a summary function.
- [`Smooth(`*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/Smooth.wmppp.md)
  : Spatial smoothing of individual dbmss's
- [`as.wmppp()`](https://ericmarcon.github.io/dbmss/dev/reference/as.wmppp.md)
  : Converts data to class wmppp
- [`sharpen(`*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/generic.spatstat.md)
  [`superimpose(`*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/generic.spatstat.md)
  [`unique(`*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/generic.spatstat.md)
  [`` `[`( ``*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/generic.spatstat.md)
  : Methods for weighted, marked planar point patterns (of class wmppp)
  from spatstat
- [`is.wmppp()`](https://ericmarcon.github.io/dbmss/dev/reference/is.wmppp.md)
  : Test whether an object is a weighted, marked, planar point pattern
- [`marks(`*`<Dtable>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/marks.md)
  [`` `marks<-`( ``*`<Dtable>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/marks.md)
  [`` `marks<-`( ``*`<wmppp>`*`)`](https://ericmarcon.github.io/dbmss/dev/reference/marks.md)
  : marks method for Dtable and wmppp objects
