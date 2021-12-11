#' Calculate age based on start and stop dates
#'
#' This function makes sure that a person only becomes older on their birthdate.
#'
#' @param from birthdate
#' @param to given date
#'
#' @return numeric
#' @export
#'
#' @examples
#' birth_date <- as.Date('1978-02-22')
#' personAge(birth_date, '2021-02-22') #43
#'
personAge <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

#' Create a full date with an ordinal date
#'
#' This function is useful for formatting dates in reports - especially in the
#' header of an Rmarkdown report. Default behavior uses today's date. See
#' \code{\link{strptime}} for details on month and year code strings.
#'
#' @param x a date object to convert to a string
#' @param month_code the code used to format the month - defaults to full month
#'   name
#' @param year_code the code used to format the year - defaults to full 4 digit
#'   year
#'
#' @return a length 1 character vector
#' @export
#'
#' @examples
#' # returns nice format for today's date
#' pretty_date()
#'
#' pretty_date(as.Date('2020-03-01'))
#' # March 1st, 2020
pretty_date <- function(x = Sys.Date(), month_code = "%B", year_code = "%Y") {
  stopifnot(inherits(x, "Date"))

  date_ordinal <- scales::ordinal(as.numeric(format(x, '%d')))
  year_full <- format(x, year_code)
  month_full <- format(x, month_code)

  paste0(month_full, " ", date_ordinal, ", ", year_full)
}
