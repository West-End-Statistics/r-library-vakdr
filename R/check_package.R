#' Make sure a package is available
#'
#' This function checks to see if a package is installed an throws an error if
#' it is not.
#'
#' @param pkg_name the package to check
#' @param msg a helpful message preappended to the error to explain why package
#'   is needed
#' @param install_directions code to install the packages - defaults to
#'   `install.packages`
#'
#' @export
#'
#' @return invisibly returns TRUE if package is available otherwise throws an
#'   error
#'
#' @examples
#' check_package('base')
check_package <- function(pkg_name,
                          msg = "",
                          install_directions = glue::glue("install.packages('{pkg_name}')")) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    stop(
      glue::glue(
        msg,
        "The `{pkg_name}` package is missing. Please install with:",
        "    {install_directions}",
        .sep = "\n"
      ),
      call. = FALSE
    )

  } else {
    invisible(TRUE)
  }
}
