#' Change the angle of x axis text to 45
#' @export

theme_axis_x45 <- function(){
  theme(axis.text.x = element_text(face = "bold", angle = 45, vjust = 1, hjust = 1))
}

#' Change the angle of x axis text to 0
#' @export

theme_axis_x0 <- function(){
  theme(axis.text.x = element_text(face = "bold", angle = 0, vjust = 0.5, hjust = 0.5))
}
