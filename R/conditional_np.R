#' Conditional distribution of a multivariate normal distribution Np
#' 
#' This function can be used to obtain the conditional distribution of any Np.
#'
#' @param x is a vector with the values that are known. For example, \code{c(3, NA, 5, NA)} means that we are interested in the conditional distribution of X2 and X4 when X1=3 and X3=5.
#' @param mu is the mean vector of the Np. 
#' @param Sigma is the covariance matrix of the Np.
#'
#' @return a list with the mean and covariance matrix of the conditional distribution.
#' 
#' @examples 
#' # Consider a N3 normal distribution
#' mu <- c(3, -1, 1)
#' Sigma <- matrix(c(3, -1, 1,
#'                   -1, 1, 0,
#'                   1, 0, 2), ncol=3)
#'                   
#' # We want the conditional distribution of X2 given X1=2 and X3=0
#' conditional_np(x=c(2, NA, 0), mu=mu, Sigma=Sigma)
#' 
#' # We want the conditional distribution of X1 and X3 given X2=2
#' conditional_np(x=c(NA, 2, NA), mu=mu, Sigma=Sigma)
#'
#' @export
#' 
conditional_np <- function(x, mu, Sigma) {
  # identificando los sitios donde hay NA's
  pos1 <- which(is.na(x))
  pos2 <- which(! is.na(x))
  # creando las matrices de la particion
  s11 <- Sigma[pos1, pos1, drop=FALSE]
  s22 <- Sigma[pos2, pos2, drop=FALSE]
  s12 <- Sigma[pos1, pos2, drop=FALSE]
  s21 <- t(s12)
  # creando los vectores de la particion
  mu1 <- matrix(mu[pos1], ncol=1)
  mu2 <- matrix(mu[pos2], ncol=1)
  x2 <- matrix(x[pos2], ncol=1)
  # Calculando lo deseado
  mean  <- mu1 + s12 %*% solve(s22) %*% (x2 - mu2)
  sigma <- s11 - s12 %*% solve(s22) %*% s21
  return(list(mean=mean, sigma=sigma))
}
