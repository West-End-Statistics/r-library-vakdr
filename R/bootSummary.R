#' \code{bootSummary} calculates the empirical distribution of a statistic.
#'
#' The \code{bootSummary} function uses bootstaps to estimate the midpoint, avg
#' and confidence interval of a user defined function. It's purpose is to work
#' seamlessly within the \pkg{dplyr} framework/the pipe an allows the use of
#' bare column names.
#'
#' The user provides the name of column to summarise along with the summarise
#' function.
#'
#' The example shows how a t-test performs similarly to a bootstrap when the
#' data is normal (Group A). It's also possible to make estimates for other
#' statistitics such as the median.
#'
#' @inheritParams modelr::bootstrap
#' @param data data.frame or tibble.
#' @param var bare column name to summarise over.
#' @param .funs summarising function.  It can be a bare function name or follow
#'   the usage of \code{\link[dplyr]{funs}}.
#' @param ... grouping variables for summary statistic.
#' @param ci width of quantile interval for final summary.
#' @param na.rm should the final summarization across bootraps remove
#'   \code{NAs}?
#'
#' @return a tibble containing the name of the grouping variables and the
#'   following columns:
#'   \describe{
#'       \item{stat_mean}{The mean across bootstraps}
#'       \item{stat_mid}{The median across bootstraps}
#'       \item{stat_low}{The low quantile (e.g. 2.5\% when ci = .95)}
#'       \item{stat_high}{The high quantile (e.g. 97.5\% when ci = .95)}
#'  }
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' # Simulate some data
#' set.seed(5)
#' size <- 1000
#' test_data <- data.frame(cohort = rep(c("A", "B"), each = size),
#'                         stat = c(rnorm(size, 5, 10), exp(rnorm(size, mean = 0.1))))
#' # T Tests
#' test_data %>%
#'   filter(cohort == "A") %>%
#'   pull(stat) %>% t.test()
#' test_data %>%
#'   filter(cohort == "B") %>%
#'   pull(stat) %>% t.test()
#'
#' # Bootrap the median
#' test_data %>% bootSummary(stat, cohort)
bootSummary <- function(data, var, ..., .funs = median,
                        n = 100, ci = .95, na.rm = FALSE){
  quant_low <- (1-ci)/2
  quant_high <- (1-quant_low)

  col_var <- rlang::enquo(var)
  grouping_var <- rlang::enquos(...)

  bs_df <- data %>%
    # select only the columns needed to minimize the resources needed for the bootstrap/as.data.frame function
    select_at(vars(!!col_var, !!!grouping_var)) %>%
    modelr::bootstrap(n) %>%
    mutate(stat_summary = purrr::map(strap, function(x){
      x %>%
        as.data.frame() %>%
        group_by(!!!grouping_var) %>%
        summarise_at(.vars = vars(!!col_var),
                     .funs = .funs)
    })) %>%
    select(.id, stat_summary) %>%
    tidyr::unnest(cols = c(stat_summary))

  bs_df %>%
    group_by(!!!grouping_var) %>%
    summarise(
      stat_mean = mean(!!col_var, na.rm = na.rm),
      stat_mid = quantile(!!col_var, .5,na.rm = na.rm),
      stat_low = quantile(!!col_var, quant_low, na.rm = na.rm),
      stat_high = quantile(!!col_var, quant_high, na.rm = na.rm)
    )

}





