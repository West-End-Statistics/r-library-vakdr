.onLoad <- function(libname, pkgname){
  options(java.parameters = "-Xmx8192m")
  message("Setting default ggplot2 options...")
  ggplot2::theme_set(theme_vakdr())

  op <- options()
  op.vakdr <- list(
    vakdr.writeXLSX = "data_output.xlsx"
  )
  toset <- !(names(op.vakdr) %in% names(op))
  if(any(toset)) options(op.vakdr[toset])

  invisible()
}

.onUnload <- function(libname, pkgname){
  message("Reverting ggplot2 settings to default")
  ggplot2::theme_set(ggplot2::theme_grey())

}
