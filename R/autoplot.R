autoplot.envelope <- function(object, fmla, ..., 
    ObsColor = "black", H0Color = "red", ShadeColor = "grey75", 
    alpha = 0.3, main = NULL, xlab = NULL, ylab = NULL, 
    LegendLabels = NULL)
{
  return(autoplot.fv(object=object, fmla=fmla, ..., 
                     ObsColor=ObsColor, H0Color=H0Color, ShadeColor=ShadeColor, 
                     alpha=alpha, main=main, xlab=xlab, ylab=ylab, 
                     LegendLabels=LegendLabels))
}


autoplot.fv <- function(object, fmla, ..., 
                              ObsColor = "black", H0Color = "red", ShadeColor = "grey75", 
                              alpha = 0.3, main = NULL, xlab = NULL, ylab = NULL, 
                              LegendLabels = NULL)
{
  # Formula
  # Code adapted from spatstat.core::plot.fv
  indata <- as.data.frame(object)
  # No formula
  defaultplot <- missing(fmla) || is.null(fmla)
  if(defaultplot) 
    fmla <- stats::formula(object)
  # May be a string: convert it to a formula
  fmla <- stats::as.formula(fmla, env=parent.frame())
  # expand "."
  umap <- spatstat.core::fvexprmap(object)
  fmla.expanded <- eval(substitute(substitute(fom, um), list(fom=fmla, um=umap)))
  # extract LHS and RHS of formula
  lhs <- fmla.expanded[[2]]
  rhs <- fmla.expanded[[3]]
  # extract data 
  lhsdata <- eval(lhs, envir=indata)
  if (is.vector(lhsdata)) {
    # Single column: must be a dataframe
    lhsdata <- data.frame(lhsdata)
    colnames(lhsdata) <- as.character(lhs)
  }
  rhsdata <- eval(rhs, envir=indata)
  alldata <- data.frame(x=rhsdata, lhsdata)
  datacols <- colnames(alldata)
  # Name of the function. Attribute may be a vector ("K" "inhom")
  fname <- paste(attr(object, "fname"), collapse = "")
  
  if (is.null(xlab)) {
    if (rhs == "r") {
      # x label is Distance (unit)
      xlab <- "Distance"
      if (attr(object, "unit")$plural != "units") {
        xlab <- paste(xlab, " (", attr(object, "unit")$plural, ")", sep="") 
      }
    } else {
      # x is not r
      xlab <- as.character(rhs)
    }
  }
  
  if (is.null(ylab)) {
    # Y label is the function's name.
    ylab <- fname
    if(!defaultplot)
      # Complete function name with the formula
      ylab <- parse(text=gsub("\\.", ylab, as.character(attr(terms(fmla), which = "variables")[2])))
  }
  
  if (is.null(LegendLabels) || is.na(LegendLabels[3])) {
    # Guide of the confidence envelope
    LegendLabels[3] <- "Confidence enveloppe"
  }
    
  # Initialize the plot
  thePlot <- ggplot2::ggplot() +
    ggplot2::labs(title=main, x=xlab, y=ylab)
  
  # Variable names
  shade <- attr(object, which="shade")
  fvs <- setdiff(datacols, c(shade, "x"))
  # Data columns containing shade must be named lo and hi
  if (!is.null(shade) & all(shade %in% datacols)) {
    # Plot envelope iif shade exists and columns have not been removed by formula
    colnames(alldata)[which(datacols==shade[1])] <- "lo"
    colnames(alldata)[which(datacols==shade[2])] <- "hi"
    # Confidence envelope
    thePlot <- thePlot +
      ggplot2::geom_ribbon(data=alldata, ggplot2::aes_(x=~x, ymin=~lo, ymax=~hi, fill=ShadeColor), alpha=alpha) +
      ggplot2::scale_fill_identity(name=LegendLabels[3], guide="legend", labels=paste(attr(object, "einfo")$Alpha*100, "%", sep=""))
  }

  # Melt observed and expected values to prepare geom_line
  Lines <- reshape2::melt(alldata, id.vars="x", measure.vars=fvs)
  # Delete NAs created by the formula to avoid warnings when plotting
  Lines <- Lines[!is.na(Lines$value), ]
  # Get variable full names and use them
  if (is.null(LegendLabels) || any(is.na(LegendLabels[1:2]))) {
    # Describe function values by their names
    levels(Lines$variable) <- vapply(levels(Lines$variable), function(fvalue) {
      sprintf(attr(object, which="desc")[which(fvs == fvalue)+1], fname)
    }, FUN.VALUE = "")
  } else {
    levels(Lines$variable) <- LegendLabels[1:2]
  }


  # Add lines to the plot
  thePlot <- thePlot +
    ggplot2::geom_line(data=Lines, ggplot2::aes_(x=~x, y=~value, colour=~variable, linetype=~variable)) +
    # Merged legend if name and labels are identical
    ggplot2::scale_colour_manual(name=ylab, values=c(ObsColor, H0Color)) +
    ggplot2::scale_linetype_manual(name=ylab, values=c(1, 2))
  
  return(thePlot)
}
