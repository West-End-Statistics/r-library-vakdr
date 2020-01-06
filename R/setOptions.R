#'Default Options for automatic loading of vakdr
#'
#'The functions below represent default options for graphing/knitting that
#'should be used when creating any report.
#'
#'
#'\describe{
#'
#'\item{`theme_vakdr`}{ A modified version of \code{ggplot2::theme_minimal()}.
#'See source code of theme_vakdr for more information, and \code{\link[ggplot2]{theme}}
#'for details.}
#'
#'}
#'
#'
#'
#'@param ... passed to theme_minimal
#'@return a ggplot2 theme object
#'@export
#'
#'@import ggplot2
#'
#'@examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = hp, y = mpg, color = as.factor(cyl))) + geom_point()
#' # Revert back to default ggplot2 theme:
#' theme_set(theme_gray())
#' ggplot(mtcars, aes(x = hp, y = mpg, color = as.factor(cyl))) + geom_point()
#'
theme_vakdr <- function(...){
  theme_minimal(...) %+replace%
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "grey20"))
}
