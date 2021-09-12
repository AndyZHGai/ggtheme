#' Modify the grid of ggplot2
#'
#' @param colour grid line colour
#' @param linetype grid line type, one of 0, 1, 2, 3
#'
#' @return
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' theme_grid(colour = "gray90", linetype = 0)
theme_grid <- function(colour = "gray90", linetype = 1){
  ggplot2::theme(
    panel.grid.major = element_line(colour = colour,
                                    size = 0.1,
                                    linetype = linetype),
    panel.grid.minor = element_line(colour = colour,
                                    size = 0.1,
                                    linetype = linetype)
  )
}
