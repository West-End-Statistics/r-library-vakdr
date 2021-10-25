#' Relabel factors using a named vector
#'
#' @param x a character or factor vector to modify
#' @param lkp_vals either a named vector where the names are the values in x and
#'   the values are the new levels or a character vector defining the order of
#'   the values of x.
#' @param ... additional parameters passed to \code{factor()}
#' @param check_names check that all the names exist
#'
#' @return a factor
#' @export
#'
#' @examples
#' x <- c("apple", "bear", "banana", "dear")
#' # new value is "fruit"
#' levels <- c("apple" = "fruit", "banana" = "fruit")
#' lkp_to_factor(x, levels)
#'
#' # just reorder factors
#' lkp_to_factor(x, c("dear", "banana", "apple", "bear"))
lkp_to_factor <- function(x, lkp_vals, ..., check_names = TRUE){

  assertthat::assert_that(is.character(lkp_vals))
  key_vals <- names(lkp_vals)

  if(!is.null(key_vals)){
    lkp_vals <- unname(lkp_vals)
  }else{
    key_vals <- lkp_vals
  }

  if(check_names){

    check_set <- function(x, possible_values, remove_na = TRUE, ...){
      x <- unique(x)
      if(remove_na){
        x <- na.omit(x)
      }

      invisible(assertthat::assert_that(setequal(x, possible_values), ...))
    }

    check_set(x, key_vals, msg = paste0("Issue with input: ", deparse(substitute(x))))
  }


  factor(x, levels = key_vals, labels = lkp_vals, ...)
}
