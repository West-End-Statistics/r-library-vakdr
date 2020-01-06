#' Copy a data.frame to clipboard for pasting into Excel
#'
#' This function wraps around \code{write.table} to push a data frame to
#' clipboard. It is pipable. Please note that large data.frames will cause the
#' clipboard to crash.
#'
#' @param ... other parameters passed to write.table
#' @inheritParams utils::write.table
#'
#' @return NULL
#' @export
#'
#' @examples
#' toClipboard(mtcars)
toClipboard <- function(x,file = "clipboard", sep = "\t", row.names = FALSE, na = "", ...){
  x <- as.data.frame(x)
  write.table(x, file =file, sep = sep, row.names = row.names, na = na)
}
