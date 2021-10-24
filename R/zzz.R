.onLoad <- function(libname, pkgname){
  options(java.parameters = "-Xmx8192m")

  op <- options()
  op.vakdr <- list(
    vakdr.writeXLSX = "data_output.xlsx"
  )
  toset <- !(names(op.vakdr) %in% names(op))
  if(any(toset)) options(op.vakdr[toset])

  invisible()
}
