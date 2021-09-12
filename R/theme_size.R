#' Change the size of the ggplot
#'
#' @param size a numeric, the default value is 14
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' theme_size(size = 14)
theme_size <- function(size = 14){
  ggplot2::theme(
    axis.text = element_text(size = size),
    axis.title = element_text(size = size*1.15),
    legend.text = element_text(size = size),
    legend.title = element_text(size = size*1.1),
    strip.text = element_text(size = size)
  )
}
