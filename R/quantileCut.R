#' quantileCut cuts a numeric vector into the a prespecified number of breaks.
#'
#' This function is useful for breaking up data into equal unique parts
#'
#' @param x a numeric vector
#' @param number_breaks how many groups should there be
#' @param force_unique should you make sure the breaks are unique?
#' @param silent will not warn if breaks aren't unique
#' @param na.rm will na's be removed?
#' @param ... passed to \code{\link{cut}}
#'
#' @return a factor (result of \code{cut})
#' @export
#'
#' @examples
#' library(dplyr)
#' tibble(original = rnorm(999)) %>%
#'   mutate(grouped = quantileCut(original)) %>%
#'   group_by(grouped) %>%
#'   summarise(MEAN = mean(original),
#'             MEDIAN = median(original),
#'             SD = sd(original),
#'             CT = n())
quantileCut <- function(x, number_breaks = 10, force_unique = TRUE, silent = TRUE, na.rm = FALSE, ...){
  breaks <- quantile(x, seq(0,1, length.out = number_breaks + 1), na.rm = na.rm)
  if(force_unique){
    breaks <- unique(breaks)
  }

  if(((length(breaks)-1) != number_breaks) & !silent){
    warning("Only ", length(breaks) - 1, " unique breaks")
  }

  cut(x, breaks = breaks, include.lowest =T, ...)

}
