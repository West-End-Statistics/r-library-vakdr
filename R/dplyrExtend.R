#' rollup add a total row to a dataframe
#'
#' This function is inteded to extend the `summarise` function in dplyr by
#' binding (using `bind_row`) a "Total" row to the dataframe. This is especially
#' useful when generating reports that require a summary line. It also
#' appropriatly scales everything to make sure that aggregate functions such as
#' `mean` are taken on the raw data and not the aggregate data.
#'
#' @inheritParams dplyr::summarise
#' @param label  what should the total row be labeled as? Only valid if the
#'   first grouping variables is a factor or character.
#'
#' @return An object of the same class as .data. One grouping level will be
#'   dropped.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # cyl is a number so it's not possible to label the "Total Row"
#' mtcars %>%
#'   group_by(cyl) %>%
#'   rollup(mean = mean(disp), n = n())
#'
#' # Casting cyl to a character allows automatic labeling of the final row.
#' mtcars %>%
#'   mutate(cyl = as.character(cyl)) %>%
#'   group_by(cyl) %>%
#'   rollup(sum = sum(disp),
#'          mean = mean(disp),
#'          sd = sd(disp),
#'          n = n())
#'
#' # Set label = NA to not label the last row (same as if it's a number)
#' mtcars %>%
#'   mutate(cyl = as.character(cyl)) %>%
#'   group_by(cyl) %>%
#'   rollup(sum = sum(disp),
#'          mean = mean(disp),
#'          sd = sd(disp),
#'          n = n(),
#'          label = NA)
rollup <- function(.data, ..., label = "Total"){
  label_col <-dplyr::group_vars(.data)[[1]]
  total_row <- dplyr::summarise(ungroup(.data), ...)

  if(inherits(.data[[label_col]], what = c("character", "factor"))){
    total_row[[label_col]] <- label
  }

  dplyr::bind_rows(
    dplyr::summarise(.data, ...),
    total_row
  )
}
