# Carte des wapas de Paracou 6 pour le sticker ####
load(".#Extra/Sticker/Paracou.RData")
library("tidyverse")
(
  Paracou %>% filter(Plot == 6 & Genus == "Eperua") %>%
    ggplot() +
    geom_point(
      aes(
        x = Xfield,
        y = Yfield,
        size = CircCorr,
        color = Species,
        stroke = 0
      )
    ) +
    coord_fixed() + scale_color_brewer(palette = "Set1") +
    labs(
      x = "",
      y = "",
      caption = ""
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.border = element_rect(colour = "black", size = 1.5), 
    ) +
    theme(legend.position = "none") ->
    P6
)
ggsave(".#Extra/Sticker/dbmss_P6.eps", plot = P6)

sticker_autoplot <- function(object, ..., ObsColor = "black", H0Color = "red", ShadeColor = "grey75", alpha=0.5, main = NULL, xlab = NULL, ylab = NULL, LegendLabels = c("Observed", "Expected", "Confidence\n enveloppe"))
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
    ggplot2::geom_line(ggplot2::aes_(x=~r, y=~value, colour=~variable, linetype=~variable, size=0.75), data=Lines) +
    # Merged legend if name and labels are identical
    ggplot2::scale_colour_manual(name=ylab,  values=c(ObsColor, H0Color), labels=LegendLabels[1:2]) +
    ggplot2::scale_linetype_manual(name=ylab,  values=c(1, 2), labels=LegendLabels[1:2])
  
  return(thePlot)
}


# Courbe de Kd ####
library("dbmss")
(KdEnvelope(paracou16,
           ReferenceType = "Q. Rosea",
           NumberOfSimulations = 100) %>%
  sticker_autoplot +
  theme_bw(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  ) +
  theme(legend.position = "none") ->
  Kd)
ggsave(".#Extra/Sticker/dbmss_Kd.eps", plot = Kd, width=4, height=3)
