#' log2fact converts all logical columns in a dataframe to a 2-level factor.
#'
#' This is useufl for on-the-fly conversion for data profiling
#'
#' @param df a dataframe where you want to convert
#'
#' @return the full df with factor columns instead of logical.
#' @export
#'
#' @examples
#' d <- data.frame(a = rnorm(100),
#'                 b = sample(100, c(T, F)))
#' log2fact(d)
#'
log2fact <- function(df,
                     positive = "Yes",
                     negative = "No") {
  bools <- sapply(df, is.logical)

  if (sum(bools) == 0) {
    stop("No logical columns found in the dataset")
  }
  if (sum(bools) == 1) {
    df[, bools] <- as.factor(ifelse(df[, bools], positive, negative))

  } else {
    df[, bools] <- lapply(df[, bools], function(x) {
      as.factor(ifelse(x, positive, negative))
    })
  }
  df
}
