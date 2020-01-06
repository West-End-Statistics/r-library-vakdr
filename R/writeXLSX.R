#' writeXLSX is a convenient way to write dataframes to xlsx files.
#'
#' The default options write a data frame to sheet named after the object name
#' in a file `data_output.xlsx`. You can write many sheets to the same file by
#' simply passing either differently named objects or passing a different name
#' for the sheet. Note that sheets will be overwritten without warning. New
#' sheet wil always be added to a file (old sheets are not cleared out).
#'
#' @param file the file name to write to
#' @param sheetName the sheet name to write the data frame to
#' @param addFilter should an auto filter be added to each column heading?
#' @param autoSize should each column be autosized?
#'
#' @inheritParams xlsx::addDataFrame
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' writeXLSX(mtcars)
#' }
writeXLSX <-
  function(x,
           file = getOption("vakdr.writeXLSX"),
           sheetName = deparse(substitute(x)),
           col.names = TRUE,
           row.names = FALSE,
           showNA = FALSE,
           addFilter = TRUE,
           autoSize = TRUE) {

    old_disp_env <- Sys.getenv("DISPLAY")
    Sys.unsetenv("DISPLAY")

    if(!require(xlsx)) stop("Missing xlsx library")

    sheetName = as.character(sheetName)
    x <- as.data.frame(x)
    total_cols_ref <- as.integer(ncol(x) - 1)

    if(file.exists(file)){
      wb <- xlsx::loadWorkbook(file)
    }else{
      wb <- xlsx::createWorkbook()
    }

    capture.output(sheets <- names(xlsx::getSheets(wb)))
    if(sheetName %in% sheets){
      xlsx::removeSheet(wb, sheetName)
    }

    sheet <- xlsx::createSheet(wb, sheetName = sheetName)

    xlsx::addDataFrame(x, sheet,
                       col.name = col.names,
                       row.names = row.names,
                       showNA = showNA)


    if (addFilter) {
      firstCell <- sheet$getRow(0L)$getCell(0L)$getReference()
      lastCell <- sheet$getRow(0L)$getCell(total_cols_ref)$getReference()
      xlsx::addAutoFilter(sheet, paste(firstCell,lastCell, sep = ":"))
    }

    if (autoSize) {
      xlsx::autoSizeColumn(sheet, 1:ncol(x))

    }

    invisible(xlsx::saveWorkbook(wb, file = file))
    Sys.setenv("DISPLAY" = old_disp_env)
  }
