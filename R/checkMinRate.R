#' Include only the risk factors that appear in x\% of the data.
#'
#' \code{checkMinRate} returns a logical value after evaluating an object whose existence is greater than a specified amount.
#' Despite permitting any object class to be passed, only an object
#' with a class of 'logical'will be evaluated.
#'
#' @param x The object to be evaluated.
#' @param min_rate The threshold for permissable risk factor found in patient population. Defaults to 1\%.
#' @return Returns variables that meet the min_rate criteria.
#' @examples
#' data <- data.frame(diab = c(T, T, F, F), obese = c(T, F, T, F), site = c("A", "B", "C", "D"),
#'      bmi = c(16, 18, 20, 22))
#' checkMinRate(data$diab, min_rate = .45)
#' @export
#'

checkMinRate <- function(x, min_rate = .01){
  if(!is.logical(x))
  {TRUE}
  else
  {mean(x, na.rm = T) >=min_rate}
}
