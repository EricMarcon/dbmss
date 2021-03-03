autoplot.envelope <- function(object, fmla, ..., 
    ObsColor = "black", H0Color = "red", ShadeColor = "grey75", 
    alpha=0.3, main = NULL, xlab = NULL, ylab = NULL, 
    LegendLabels = c("Observed", "Expected", "Confidence\n enveloppe"))
{
  if (is.null(xlab)) {
    # X label is Distance (unit)
    xlab <- "Distance"
    if (attr(object, "unit")$plural != "units") {
      xlab <- paste(xlab, " (", attr(object, "unit")$plural, ")", sep="") 
    }
  }

  # Formula
  # Code from spatstat.core::plot.fv
  indata <- as.data.frame(object)
  # No formula
  defaultplot <- missing(fmla) || is.null(fmla)
  if(defaultplot) 
    fmla <- stats::formula(object)
  fmla <- stats::as.formula(fmla, env=parent.frame())
  # Extract left hand side as given
  fmla.original <- fmla
  # expand "."
  umap <- fvexprmap(object)
  fmla <- eval(substitute(substitute(fom, um), list(fom=fmla, um=umap)))
  # extract LHS and RHS of formula
  lhs <- fmla[[2]]
  rhs <- fmla[[3]]
  # extract data 
  lhsdata <- eval(lhs, envir=indata)
  rhsdata <- eval(rhs, envir=indata)
  alldata <- data.frame(r=rhsdata, lhsdata)
  
  if (is.null(ylab)) {
    # Y label is the function's name
    ylab <- attr(object, "fname")
    if(!defaultplot)
      ylab <- parse(text=gsub("\\.", ylab, as.character(attr(terms(fmla.original), which = "variables")[2])))
  }
  
  # Confidence envelope
  thePlot <- ggplot2::ggplot(data=alldata) +
    ggplot2::geom_ribbon(ggplot2::aes_(x=~r, ymin=~lo, ymax=~hi, fill=ShadeColor), alpha=alpha) +
    ggplot2::labs(title=main, x=xlab, y=ylab) + 
    ggplot2::scale_fill_identity(name=LegendLabels[3], guide="legend", labels=paste(attr(object, "einfo")$Alpha*100, "%", sep="")) 
  
  # Melt observed and expected values to prepare geom_line
  if (is.null(alldata$mmean)) {
    measure.vars <- c("obs", "theo")
  } else {
    measure.vars <- c("obs", "mmean")
  }
  # Melted dataframe
  Lines <- reshape2::melt(alldata, id.vars="r", measure.vars=measure.vars)
  # Delete NAs created by the formula to avoid warnings when plotting
  Lines <- Lines[!is.na(Lines$value), ]
  
  # Add lines to the plot
  thePlot <- thePlot +
    ggplot2::geom_line(data=Lines, ggplot2::aes_(x=~r, y=~value, colour=~variable, linetype=~variable)) +
    # Merged legend if name and labels are identical
    ggplot2::scale_colour_manual(name=ylab,  values=c(ObsColor, H0Color), labels=LegendLabels[1:2]) +
    ggplot2::scale_linetype_manual(name=ylab,  values=c(1, 2), labels=LegendLabels[1:2])

  return(thePlot)
}
