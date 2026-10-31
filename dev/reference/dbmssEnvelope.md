# Class of envelope of function values (fv)

A class `"dbmssEnvelope"`, *i.e.* a particular type of see
[`envelope`](https://rdrr.io/pkg/spatstat.explore/man/envelope.html) to
represent several estimates of the same function and its confidence
envelope.

## Details

"dbmssEnvelope" objects are similar to envelope objects. The differences
are that the risk level is chosen (instead of the simulation rank to use
as the envelope), so the rank is calculated (interpolation is used if
necessary), and a global envelope can be calculated following Duranton
and Overman (2005).

## References

Duranton, G. and Overman, H. G. (2005). Testing for Localisation Using
Micro-Geographic Data. *Review of Economic Studies* 72(4): 1077-1106

## See also

[`summary.dbmssEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/summary.dbmssEnvelope.md),
[`KdEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/KdEnvelope.md),
[`MEnvelope`](https://ericmarcon.github.io/dbmss/dev/reference/MEnvelope.md)
