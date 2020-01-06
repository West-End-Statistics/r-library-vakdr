#' runLocalScript returns a tidy output for a file that was run
#'
#' This function returns a tibble with all of the information available from
#' running the file. Specifically it helps capture the standard error/output by
#' dumping to a temporary file and reading it back in. It's also possible to
#' specify the std_out and std_error files if desired.
#' @inheritParams sys::r_wait
#' @param x a path to a file to run
#' @param error_on_missing if FALSE then the function catches errors and returns
#'   a reasonable tibble otherwise an error is thrown.
#' @param std_err either NULL (writes to temp file) or values allowed by \link[sys]{r_wait}
#' @param std_out either NULL (writes to temp file) or values allowed by \link[sys]{r_wait}
#' @return a tibble with the following columns:
#'     status - the exit status of the script
#'     std_error - the output of std_error
#'     std_out - the output of std_out
#'     time_taken - the amount of time it took to run the function
#' @export
runLocalScript <- function(x,
                           args = "--vanilla",
                           std_err = NULL,
                           std_out = NULL,
                           error_on_missing = FALSE) {

  if(!file.exists(x)){
    err_msg <- paste("File", x, "is missing")

    if(error_on_missing){
      stop(err_msg)
    }else{
      output <- tibble::tibble(
        status = 1,
        std_error = err_msg,
        std_out = NA_character_,
        time_taken = NA_real_
      )
      return(output)


    }

  }

  if(is.null(std_err)){
    std_err <- tempfile(fileext = '.txt')
  }

  if(is.null(std_out)){
    std_out <- tempfile(fileext = '.txt')
  }



  start_time <- Sys.time()
  status <- sys::r_wait(std_in = x,
                        std_err = std_err,
                        std_out = std_out,
                        args = args)
  end_time <- Sys.time()

  std_error <- paste0(readLines(std_err), collapse = "\n")
  std_out <- paste0(readLines(std_out), collapse = "\n")

  tibble::tibble(
    status = status,
    std_error = std_error,
    std_out = std_out,
    time_taken = as.numeric(end_time - start_time, unit = "secs")
  )
}


