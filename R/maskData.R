#' maskData obfuscates a column vector
#'
#' maskData maps the current values in a column (such as names) and maps them to
#' either 1 or 2 levels. This is useful for quickly changing surgeon names in a
#' report so that the surgeon identity is protected. The original values are set
#' to the names of the newly created character vector. When there are less than
#' 26^4 values then LETTERS are used but otherwise a numeric index is applied.
#'
#' @param x column vector of data to mask
#' @param header character string to preappend to each value
#' @param randomize should the mask be applied randomly?
#'
#' @return a named character vector
#' @export
#'
#' @examples
#' test_df <- data.frame(original = sample(paste0("SURG",1:total_surgeons),30,
#' replace = TRUE),
#' stringsAsFactors = FALSE)
#'
#' test_df <- dplyr::mutate(test_df, new = maskData(original))
#' test_df
maskData <- function(x, header = "", randomize = TRUE){
  unique_x <- unique(x)
  len_un_x <- length(unique_x)


  required_num <- ceiling(log(len_un_x)/log(26))
  if(required_num < 5) {
      newvals <- purrr::reduce(rep(list(LETTERS), required_num),
                    ~as.vector(outer(.x, .y, FUN = "paste0")))
  } else {
    newvals <- as.character(1:len_un_x)
  }

  selecter <- 1:len_un_x
  if(randomize) selecter <- sample(selecter)

  lkp_vals <- paste0(header, newvals[selecter])
  names(lkp_vals) <- unique_x

 out <- lkp_vals[x]
 names(out) <- x

 out
}

