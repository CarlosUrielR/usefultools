#' Log-likelihood
#' 
#' This function calculates the log-likelihood at some parameter values given the random sample and distribution.
#' 
#' @param x numeric vector.
#' @param dist density name for the distribution in the form \code{dxxx}. By default it's value is \code{dnorm} but the user can use any density.
#' @param param list with the parameter values at which can be calculated the log-likelihood. 
#' 
#' @examples 
#' # For Poisson distribution
#' x <- rpois(n=50, lambda=4.5)
#' loglik(x, dist='dpois', param=list(lambda=4))
#' loglik(x, dist='dpois', param=list(lamb=4.5))
#' loglik(x, dist='dpois', param=list(lambda=5))
#' 
#' # For normal distribution
#' y <- rnorm(n=100, mean=170, sd=10)
#' loglik(x=y, dist='dnorm', param=list(mean=170, sd=10)) # Evaluated at true theta
#' loglik(x=y, dist='dnorm', param=list(mean=150, sd=20)) # Evaluated at any theta
#' 
#' @export
#'
loglik <- function(x, dist='dnorm', param=list(mean=0, sd=1)) {
  ll <- do.call(dist, c(list(x=x), param, log=TRUE))
  return(sum(ll))
}