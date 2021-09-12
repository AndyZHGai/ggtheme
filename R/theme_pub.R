#' ggplot theme for publication ready Plots
#'
#' @param base_size the default value is 14
#' @param base_family the default value is "sans"
#'
#' @return
#' @export
#'
#' @author Koundinya Desiraju
#' @examples
#' gglm::gglm() + ggtheme::theme_pub()
#' gglm(group = "Species", label.y = 0.99) + ggtheme::theme_pub() +
#' ggprism::scale_color_prism(palette = "floral")
theme_pub <- function(base_size = 14, base_family = "sans") {
  library(ggplot2)
  ggthemes::theme_foundation(base_size = base_size, base_family=base_family) +
    theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5,
                                    margin = margin(0,0,20,0)),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line.x = element_line(colour = "black"),
            axis.line.y = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            legend.margin = margin(0, 0, 0, 0),
            legend.title = element_text(face = "italic"),
            plot.margin=unit(c(10, 5, 5, 5), "mm"),
            strip.background=element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
            strip.text = element_text(face = "bold")
    )
}