autoplot.envelope <- function(object, ..., ObsColor = "black", H0Color = "red", ShadeColor = "grey75", alpha=0.3, main = NULL, xlab = NULL, ylab = NULL, LegendLabels = c("Observed", "Expected", "Confidence\n enveloppe"))
{
  if (is.null(xlab)) {
    # X label is Distance (unit)
    xlab <- "Distance"
    if (attr(object, "unit")$plural != "units") {
      xlab <- paste(xlab, " (", attr(object, "unit")$plural, ")", sep="") 
    }
  }
   
  if (is.null(ylab)) {
    # Y label is the function's name
    ylab <- attr(object, "fname")
  }
  
  # Confidence envelope
  thePlot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes_(x=~r, ymin=~lo, ymax=~hi, fill=ShadeColor), data=object, alpha=alpha) +
    ggplot2::labs(title=main, x=xlab, y=ylab) + 
    ggplot2::scale_fill_identity(name=LegendLabels[3], guide="legend", labels=paste(attr(object, "einfo")$Alpha*100, "%", sep="")) 
  
  # Melt observed and expected values to prepare geom_line
  if (is.null(object$mmean)) {
    measure.vars <- c("obs", "theo")
  } else {
    measure.vars <- c("obs", "mmean")
  }
  # Melted dataframe
  Lines <- reshape2::melt(object, id.vars="r", measure.vars=measure.vars)
  
  # Add lines to the plot
  thePlot <- thePlot +
    ggplot2::geom_line(ggplot2::aes_(x=~r, y=~value, colour=~variable, linetype=~variable), data=Lines) +
    # Merged legend if name and labels are identical
    ggplot2::scale_colour_manual(name=ylab,  values=c(ObsColor, H0Color), labels=LegendLabels[1:2]) +
    ggplot2::scale_linetype_manual(name=ylab,  values=c(1, 2), labels=LegendLabels[1:2])

  return(thePlot)
}
