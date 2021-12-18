#' Helper functions to convert probabilities and odds
#'
#' @param p a probability
#' @param x an odds
#' @param lx a log-odds
#'
#' @return a numeric vector
#' @export
#' @examples
#' prob2odds(.5)
#' odds2prob(prob2odds(.5))
prob2odds <- function(p){
  p/(1-p)
}

#' @export
#' @describeIn prob2odds convert to logodds
prob2logodds <- function(p){
  log(prob2odds(p))
}

#' @export
#' @describeIn prob2odds convert odds to probability
odds2prob <- function(x){
  x/(1+x)
}

#' @export
#' @describeIn prob2odds convert logodds to probability
logodds2prob <- function(lx){
  odds2prob(exp(lx))
}
