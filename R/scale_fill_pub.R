#' The color palette is also designed with the help of color brewer using bold and contrasting colors so, one can easily distinguish any two colors .
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @author Koundinya Desiraju
#' @examples
#' ggplot(mtcars, aes(factor(carb),fill=factor(carb))) + geom_bar(alpha=0.7) + ggtheme::theme_pub() +
#'  ggtheme::scale_fill_pub()
scale_fill_pub <- function(...){
  library(ggplot2)
  discrete_scale("fill","Publication",
                 scales::manual_pal(values = c("#386cb0","#f87f01", "#7fc97f","#ef3b2c","#feca01",
                                       "#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)

}

#' The color palette is also designed with the help of color brewer using bold and contrasting colors so, one can easily distinguish any two colors .
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @author Koundinya Desiraju
#' @examples
#' gglm(group = "Species", label.y = 0.99) + ggtheme::theme_pub() +
#'  ggtheme::scale_colour_pub()
scale_colour_pub <- function(...){
  library(ggplot2)
  discrete_scale("colour","Publication",
                 scales::manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01",
                                       "#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)

}
