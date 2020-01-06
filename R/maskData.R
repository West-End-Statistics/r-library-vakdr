#' maskData obfuscates a column vector
#'
#' maskData maps the current values in a column (such as names) and maps them to
#' either 1 or 2 levels. This is useful for quickly changing surgeon names in a
#' report so that the surgeon identity is protected. The orignal values are set
#' to the names of the newly created character vector. This function can map up
#' to 26^2 (676) values.
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

  if(len_un_x <= 26){
    newvals <- LETTERS
  }else if(len_un_x > 26 & len_un_x <= 26^2){
    newvals <- as.vector(outer(LETTERS, LETTERS, paste0))
  }else{
    stop("maskData cannot uniquely define >", 26^2, " unique values.")
  }

  selecter <- 1:len_un_x
  if(randomize) selecter <- sample(selecter)

  lkp_vals <- paste0(header, newvals[selecter])
  names(lkp_vals) <- unique_x

 out <- lkp_vals[x]
 names(out) <- x

 out
}
