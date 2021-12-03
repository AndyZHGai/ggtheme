#' GraphPad Prism themes
#'
#' @param base_size `numeric`. Base font size, given in `"pt"`.
#' @param base_family `string`. Base font family, default is `"Times New Roman"`.
#' @param angle `integer`. Angle of x_axis text in degrees.
#' One of: `0, 45, 90, 270`.
#' @param border `logical`. Should a border be drawn around the plot?
#' Clipping will occur unless e.g. `coord_cartesian(clip = "off")` is used.
#'
#' @return Returns a list-like object of class _theme_.
#'
#' @export
theme_prism <- function(base_size = 16,
                        base_family = "Times New Roman",
                        angle = 45,
                        border = FALSE) {
  base_fontface = "bold"
  # Ensure x axis text is at a sensible angle
  angle <- angle[1]
  if(!angle %in% c(-30, 0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(-30, 0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
         call. = FALSE)

  # Draw border or not
  if(!is.logical(border)) {
    stop("border must be either: TRUE or FALSE")
  } else {
    if(border){
      panel.border <- element_rect(fill = NA)
      axis.line <- element_blank()
    }
    else if (!border) {
      panel.border <- element_blank()
      axis.line <- element_line()
    }
  }

  t <- theme(
    # Base elements (to be inherited by other elements)
    line = element_line(colour = "black", size = 1, linetype = 1, lineend = "square"),
    rect = element_rect(fill = "white", colour = "black", size = 1, linetype = 1),
    text = element_text(family = base_family, face = base_fontface,
                        colour = "black", size = base_size,
                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                        margin = margin(), debug = FALSE),

    # Prism custom theme elements
    prism.ticks.length = unit(base_size / 5, "pt"),

    # Normal ggplot2 theme elements
    axis.line =          axis.line,
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.95), colour = "black"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * base_size / 4),
                                      angle = angle,
                                      hjust = ifelse(angle %in% c(45, 90, 270), 1, 0.5),
                                      vjust = ifelse(angle %in% c(-30, 0, 90, 270), 0.5, 1)),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * base_size / 4), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.5 * base_size / 4), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.5 * base_size / 4), hjust = 0),
    axis.ticks =         element_line(),
    axis.ticks.length =  unit(base_size / 2.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(margin = margin(t = base_size * 0.6), vjust = 1),
    axis.title.x.top =   element_text(margin = margin(b = base_size * 0.6), vjust = 0),
    axis.title.y =       element_text(angle = 90,
                                      margin = margin(r = base_size * 0.6),
                                      vjust = 1),
    axis.title.y.right = element_text(angle = -90,
                                      margin = margin(l = base_size * 0.6),
                                      vjust = 0),

    legend.background =  element_blank(),
    legend.spacing =     unit(base_size, "pt"),
    legend.spacing.x =   NULL,
    legend.spacing.y =   NULL,
    legend.margin =      margin(0, 0, 0, 0),
    legend.key =         element_blank(),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   unit(base_size * 1.8, "pt"),
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_blank(),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(base_size, "pt"),

    panel.background = element_rect(fill = NA, colour = NA),
    panel.border =       panel.border,
    panel.grid =         element_blank(),
    panel.grid.minor =   element_blank(),
    panel.spacing =      unit(base_size / 2, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_blank(),
    strip.text =         element_text(colour = "black",
                                      size = rel(1),
                                      margin = margin(base_size / 2.5, base_size / 2.5,
                                                      base_size / 2.5, base_size / 2.5)),
    strip.text.x =       element_text(margin = margin(b = base_size / 3)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = base_size / 3)),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(base_size / 4, "pt"),
    strip.switch.pad.wrap = unit(base_size / 4, "pt"),

    plot.background =    element_rect(fill = NA, colour = NA),
    plot.title =         element_text(size = rel(1.2),
                                      hjust = 0.5, vjust = 1,
                                      margin = margin(b = base_size)),
    plot.title.position = "panel",
    plot.subtitle =      element_text(hjust = 0.5, vjust = 1,
                                      margin = margin(b = base_size / 2)),
    plot.caption =       element_text(size = rel(0.8),
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = base_size / 2)),
    plot.caption.position = "panel",
    plot.tag =           element_text(size = rel(1.8), family = "serif",
                                      hjust = 0, vjust = -1),
    #plot.tag.position =  'topleft',
    plot.margin =        margin(rep(5, 4)),
    complete = TRUE
  )
}
