#' Simple `ifelse`
#'
#' replaceVal acts very similary to `ifelse` but with a more compact syntax for checking a column against a single value.
#'
#' @param check vector to check equality to `replace_if`
#' @param replace replacement for `check`. Recycled if necessary
#' @param replace_if currently only allows a length = 1 vector to compare to `check`
#'
#' @return vector of same type as `check`
#' @export
#'
#' @examples
#' \dontrun{
#' }
replaceValue <- function(check, replace, replace_if = NA){
  if(is.na(replace_if)){
    sel <- is.na(check)
  }else{
    sel <- check == replace_if
  }

  if(length(replace) < length(check)){
    replace <- rep_len(replace, length(check))


  }
  check[sel] <- replace[sel]

  check
}

