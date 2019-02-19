#' Quantiles manually
#'
#' This function calculates quantiles using the classroom formula.
#'
#' @param x numeric vector.
#'
#' @examples
#' # Generating a random vector
#' x <- rnorm(n=100)
#' manual_quantiles(x)
#'
#' @export
#'
manual_quantiles <- function(x){
  position <- (1:3)*(length(x)+1) / 4
  x <- sort(x)
  a <- trunc(position)
  b <- ceiling(position)
  rta <- (x[a]+x[b]) / 2
  names(rta) <- c('Q1','Q2','Q3')
  rta
}
