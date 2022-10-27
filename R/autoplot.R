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
  # Code adapted from spatstat.explore::plot.fv
  indata <- as.data.frame(object)
  # No formula
  defaultplot <- missing(fmla) || is.null(fmla)
  if(defaultplot) 
    fmla <- stats::formula(object)
  # May be a string: convert it to a formula
  fmla <- stats::as.formula(fmla, env=parent.frame())
  # expand "."
  umap <- spatstat.explore::fvexprmap(object)
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
      ylab <- parse(text=gsub("\\.", ylab, as.character(attr(stats::terms(fmla), which = "variables")[2])))
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
    if (is.null(attr(object, "einfo")$Alpha)) {
      # Envelope from spatstat
      CI <- 2 * attr(object, "einfo")$nrank / (1 + attr(object, "einfo")$nsim)*100
    } else {
      # Envelope from dbmss
      CI <- attr(object, "einfo")$Alpha*100
    }
    thePlot <- thePlot +
      ggplot2::geom_ribbon(data=alldata, ggplot2::aes(x=.data$x, ymin=.data$lo, ymax=.data$hi, fill=ShadeColor), alpha=alpha) +
      ggplot2::scale_fill_identity(name=LegendLabels[3], guide="legend", labels=paste(CI, "%", sep=""))
  }

  # Melt observed and expected values to prepare geom_line
  Lines <- reshape2::melt(alldata, id.vars="x", measure.vars=fvs)
  # Delete NAs created by the formula to avoid warnings when plotting
  Lines <- Lines[!is.na(Lines$value), ]
  
  # Color
  fvalu <- attr(object, "valu")
  col <- vapply(unique(Lines$variable), function(fvalue){
    fvalue <- as.character(fvalue)
    if (fvalue == fvalu) return(ObsColor)
    # Possible H0 function values (from spatstat)
    if (fvalue == "theo" | fvalue == "mmean") return(H0Color)
    # Other functions have the observed value color
    return(ObsColor)
  },
  FUN.VALUE = "")
  
  # Type
  other_variables <- setdiff(fvs, c(fvalu, "theo", "mmean"))
  lty <- vapply(unique(Lines$variable), function(fvalue){
    fvalue <- as.character(fvalue)
    if (fvalue == fvalu) return(1)
    if (fvalue == "theo" | fvalue == "mmean") return(2)
    # Other functions have lty = 3, 4, ...
    return(which(other_variables == fvalue)+2)
  },
  FUN.VALUE = 0)
  
  # Get variable full names and use them
  if (is.null(LegendLabels) || any(is.na(LegendLabels[1:2]))) {
    # Describe function values by their names by mapping indata cols and attribute "desc"
    levels(Lines$variable) <- vapply(levels(Lines$variable), function(fvalue) {
      sprintf(attr(object, which="desc")[which(colnames(indata) == fvalue)], fname)
    }, 
    FUN.VALUE = "")
  } else {
    levels(Lines$variable) <- LegendLabels[1:2]
  }


  # Add lines to the plot
  thePlot <- thePlot +
    ggplot2::geom_line(data=Lines, ggplot2::aes(x=.data$x, y=.data$value, colour=.data$variable, linetype=.data$variable)) +
    # Merged legend if name and labels are identical
    ggplot2::scale_colour_manual(name=ylab, values=col) +
    ggplot2::scale_linetype_manual(name=ylab, values=lty)
  
  return(thePlot)
}


autoplot.wmppp <- function(object, ..., show.window = TRUE, 
                           MaxPointTypes = 6, Other = "Other",
                           main = NULL, xlab = NULL, ylab = NULL, LegendLabels = NULL, 
                           labelSize = "Weight", labelColor = "Type", palette="Set1",
                           windowColor = "black", windowFill = "transparent", alpha = 1)
{
  # Arrange the data
  thePoints <- with(object, data.frame(x, y, PointWeight=marks$PointWeight, PointType=marks$PointType))

  # Control the point types to display
  NbPointTypes <- length(unique(thePoints$PointType))
  if (NbPointTypes > MaxPointTypes) {
    MostFrequentTypes <- sort(table(thePoints$PointType), decreasing = TRUE)[1:MaxPointTypes]
    MostFrequentTypes <- dimnames(MostFrequentTypes)[[1]]
    if (!(Other %in% levels(thePoints$PointType)))
      levels(thePoints$PointType) <- c(levels(thePoints$PointType), Other)
    for (i in 1:length(thePoints$PointType))
      if (!(thePoints$PointType[i] %in% MostFrequentTypes))
        thePoints$PointType[i] <- Other
  }

  # Plot the points
  thePlot <- ggplot2::ggplot(thePoints) +
    ggplot2::geom_point(ggplot2::aes(x=.data$x, y=.data$y, size=.data$PointWeight, color=.data$PointType), alpha=alpha) + 
    ggplot2::coord_fixed() + ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::labs(title=main, x=xlab, y=ylab, size=labelSize, color=labelColor)
  
  # Plot the window
  if (show.window) {
    if (object$window$type == "rectangle") {
      theRectangle <- data.frame(xmin=object$window$xrange[1],
                                 xmax=object$window$xrange[2],
                                 ymin=object$window$yrange[1],
                                 ymax=object$window$yrange[2])
      thePlot <- thePlot +
        ggplot2::geom_rect(theRectangle, mapping=ggplot2::aes(xmin=.data$xmin, xmax=.data$xmax, ymin=.data$ymin, ymax=.data$ymax),
                           color=windowColor, fill=windowFill, alpha=0)
    }
    if (object$window$type == "polygonal") {
      for (polygon in object$window$bdry) {
        thePolygon <- data.frame(x=polygon$x,
                                 y=polygon$y)
        thePlot <- thePlot +
          ggplot2::geom_polygon(thePolygon, mapping=ggplot2::aes(x=.data$x, y=.data$y),
                             color=windowColor, fill=windowFill, alpha=0)
      }
    }
  }
  
  return(thePlot)
}
