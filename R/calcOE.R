#' calcOE calculate CI's for O's or O/E's
#'
#' The \code{calcOE} function is a convenience function that calculates various
#' CI intervals using one of three methods: binomial, poisson or bootstrap.
#' Additionally it adds some conveniences around working with data.frames by
#' allowing the use of formulas and grouping variables.
#'
#' @section \code{binconf}:
#'   This method leverages the \code{\link[Hmisc]{binconf}} function to
#'   calculate the confidence interval around the observed proportion of events.
#'   This is the default method and benifits from returning reasonable returns
#'   when there are no events. Specifically, there are no events in a group, the
#'   point estimate of the o/e is always 0. However, the high estimate of the
#'   o/e should be related to the number of cases in the group. For example, we
#'   are more confident that the actual o/e is  closer to 0 if there are 100
#'   cases vs 10 cases.
#' @section  \code{poisson}:
#'   This is a commonly used method for calculating the confidence interval. It
#'   works by assuming the total number of observations come from a poisson
#'   distribuion and calculates the interval based on that. However, this method
#'   does not take the number of cases into account. Specifically, the CI around
#'   10 events is the same whether it came from 100 or 1,000 cases.
#' @section \code{bootstrap}:
#'  This method uses bootstraping to resample the original data and create a
#'  distribution of o/e's that can be used to directly calculate the quantiles.
#'  While this method benefits from not assuming a distribution of o's as above,
#'  it breaks down when there are no/all events. Resampling will always produce
#'  0's or 1's at high/low CI.
#'
#' @param formula a formula where the RHS contains the observed value while the
#'   LHS contains the e. If LHS = `1` then o values are not adjusted.
#' @param data a dataframe with columns specificed in groupings and formula.
#' @param ... an unquoted list of grouping variables (optional) that o's and
#'   o/e's should be calculated on.
#' @param o_vect a vector of outcomes. Should be either logical or numberic with
#'   0 and 1 as values. Factors are not supported.
#' @param e_vect a vector a probabilities. Can also be \code{NA} if a
#'   distribution on o's is desirable.
#' @param prob the width of the confidence interval.
#' @param prefix should a prefix be added to the column names?
#' @param df should the output be a dataframe or a named vector? If \code{...}
#'   is supplied then it must be true.
#' @param method which method should be used for CI estimation? See details for
#'   information on the three methods.
#' @param num_boot if \code{method = "bootstrap"} then how many bootstraps
#'   should be used?
#'
#' @return either a dataframe or named vector with the following: \describe{
#'   \item{\strong{o_e}/\strong{o}}{The point estimate of the o/e or o}
#'   \item{\strong{low_o_e}/\strong{low_o}}{the low CI based on \code{prob}}
#'   \item{\strong{high_o_e}/\strong{high_o}}{the high CI based on \code{prob}}
#'   \item{\strong{n}}{The number of observations} }
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' df_size <- 1000
#' # Create some unbalanced groups
#' groups <- sample(c("A", "B", "C"), df_size, replace = T, prob = c(.05, .35, .6))
#'
#' d_example <- data.frame(groups = groups,
#'                         e = runif(df_size)) %>%
#'   mutate(o = map_int(e, ~rbinom(1,1, .)),
#'          o2 = 0)
#'
#' # Estimates between the three methods are similar when n is reasonably sized and
#' # there are a reasonable amount of events
#' methods <- c("binconf", "poisson", "bootstrap")
#' names(methods) <-methods
#'
#' map_df(methods,
#'        ~calcOE(o ~ e, data = d_example, method =.),
#'        .id = "method")
#'
#' # However there are large difference when groups are compared that don't have events
#' # only the binomial method provides CI's that get smaller with a larger denominator
#'
#' map_df(methods,
#'        ~calcOE(o2 ~ e, data = d_example, method =., groups),
#'        .id = "method")  %>%
#'   arrange(groups)
#'
#' # It's also possible to create CI's on just the O, ignoring the expected value:
#' calcOE(o ~ 1, data = d_example, method = "binconf")
#' calcOE(o ~ 1, data = d_example, method = "poisson")
#' calcOE(o ~ 1, data = d_example, method = "bootstrap")
calcOE <- function(x,...){
  UseMethod("calcOE")

}

#' @describeIn calcOE main method for formula interface
#' @export
calcOE.formula <- function(formula, data,
                           ...,
                           prob = .75,
                           prefix = "",
                           df = TRUE,
                           method = c("binconf", "poisson" , "bootstrap"),
                           num_boot = 1000){

  method <- match.arg(method)

  # check if groups need to be created
  eval_groups <- quos(...)

  if(length(eval_groups) != 0){
    # use purrr/map to create groups for output
    out <- data %>%
      group_by(!!!eval_groups) %>%
      tidyr::nest() %>%
      mutate(calc_o_e = purrr::map(data, ~ calcOE(formula, data = ., prob = prob,
                                                  prefix = prefix,
                                                  df = TRUE,
                                                  method = method,
                                                  num_boot = num_boot))) %>%
      select(-data) %>%
      tidyr::unnest(calc_o_e)
    return(out)
  }


  if(length(formula) == 2){
    stop("No outcome variable is provided.", "\n",
         "Please be sure to include a variable to the LHS of the formula")
  }

  form_dep <- update(formula, . ~ 1)
  mod_frame <- model.frame(form_dep, data)
  if(ncol(mod_frame) != 1){
    stop("only provide one response variable (LHS)")
  }
  o_vect <- mod_frame[[1]]

  form_ind <- update(formula, . ~ . -1)
  mod_mat <- model.matrix(form_ind, data)
  if(ncol(mod_mat) == 0){
    # no e values so just calculate the CI
    e_vect <- NA
  }else if(ncol(mod_mat) > 1){
    stop("only provide one predictor variable (RHS)")
  }else{
    e_vect <- mod_mat[,1]

  }

  calcOE(o_vect, e_vect, prob = prob, prefix = prefix,
         method = method,
         num_boot = num_boot)
}

#' @describeIn calcOE basic method for calcOE and contains all measure logic.
#' @export
calcOE.numeric <- function(o_vect,
                           e_vect = NA, prob = .75, prefix = "",
                           df = TRUE,
                           method = c("binconf", "poisson" , "bootstrap"),
                           num_boot = 1000){

  method <- match.arg(method)

  if(!all(unique(o_vect) %in% c(0, 1))){
    stop("Make sure you provide a proper response (only 2 values) that are numeric/logical.", "\n",
         "Did you switch the o and e?\n",
         "Values:",  paste0(unique(o_vect), collapse = ", "))
  }

  n <- length(o_vect)
  o <- sum(o_vect)

  no_e <- all(is.na(e_vect))
  if(no_e){
    e <- 1
  }else{
    e <- sum(e_vect)
  }

  low_q <- (1-prob)/2

  if(method == "poisson"){
    low_o <- qpois(low_q, o)
    high_o <- qpois(1-low_q, o)
  }

  if(method == "binconf"){
    bin_conf_est <- Hmisc::binconf(o, n, alpha = 1-prob)*n
    low_o <- bin_conf_est[2]
    high_o <- bin_conf_est[3]
  }
  if(method == "bootstrap"){
    boot_samps <- sapply(1:1000, function(x){
      samp_ind <- sample(1:n,size = n, replace = TRUE)
      if(no_e){
        denom <- 1
      }else{
        denom <- sum(e_vect[samp_ind])
      }

      sum(o_vect[samp_ind])/denom
    })

    out <- c(o_e = median(boot_samps),
             low_o_e = quantile(boot_samps, low_q, names = F),
             high_o_e = quantile(boot_samps, 1-low_q, names = F))

  }else{
    out <- c(o_e = o/e,
             low_o_e = low_o/e,
             high_o_e = high_o/e)
  }

  out[["n"]] <- n

  if(no_e){
    names(out) <- gsub("_e", "", names(out), fixed = TRUE)
  }

  names(out) <- paste0(prefix, names(out))

  if(df){
    out <- data.frame(t(out))
  }

  out
}

#' @describeIn calcOE basic method for calcOE that converts logical to integers.
#' @export
calcOE.logical <- function(o_vect,
                           e_vect = NA, prob = .75, prefix = "",
                           df = TRUE,
                           method = c("binconf", "poisson" , "bootstrap"),
                           num_boot = 1000){

  calcOE(as.integer(o_vect), e_vect = e_vect, prob = prob, prefix = prefix,
         df = df,
         method = method,
         num_boot = num_boot)



}
