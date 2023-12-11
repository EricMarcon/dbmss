kmwmppp() is the abandonned alternative to Smooth.ppp().

It relies on ordinary kriging instead of smoothing to produce maps of dbmss individual values.

It requires importing 
  automap,
  dplyr,
  sp,
  tidyr,
  tidyselect,
in DESCRIPTION and exporting kwmppp() and plot.kwmppp():
  export("kwmppp")
  S3method("plot", "kwmppp")
