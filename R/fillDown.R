#'fillDown fills a measurement down for a specific amount of time.
#'
#'For example, if you have a lab measurement and want it to be valid for the
#'next 6 hours this function can be used to set the next 6 hours if no other
#'measurment has been made. Only NA's are filled in.
#'
#'@param meas_col column to carry forward
#'@param time_col column that defines what "time" a measurment is taken. Can be
#'  any data type that supports numeric operations.
#'@param carry_forward how long to carry a measurement forward for. Must be the
#'  same type as `time_col`
#'
#'@return vector type same as meas_col
#'@export
#'
#' @examples
#'library(dplyr)
#' set.seed(2)
#' d <- data.frame(meas = rnorm(100),
#'                 time = 1:100)
#' d$meas[sample(1:nrow(d), 70)] <- NA
#' d %>%
#'   mutate(filled_meas = fillDown(meas, time, carry_forward = 3))
fillDown <- function(meas_col, time_col, carry_forward = lubridate::hours(6)){
  # meas_col must be ordered

  missing_cases <- which(is.na(meas_col))
  missing_case_times <- time_col[missing_cases]
  missing_case_times_earliest <- missing_case_times - carry_forward


  new_meas <- sapply(seq_along(missing_case_times), function(i){
    valid_times <- time_col >= missing_case_times_earliest[i] &
      time_col <= missing_case_times[i]

    valid_meas <- meas_col[valid_times & !is.na(meas_col)]

    out <- valid_meas[length(valid_meas)]
    if(length(out) == 0){ out <- NA}

    out

    })

  final_meas <- meas_col

  final_meas[missing_cases] <- new_meas

  final_meas
}


