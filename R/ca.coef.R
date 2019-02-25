#' Coordinate points for a ca object.
#'
#' This function calculates the coordinate points for an object
#' with class \code{ca} obtained from a correspondence analysis.
#'
#' @param x A ca object.
#'
#' @return The function returns a list with two elements, coordinate points for first
#' and second dimensions.
#'
#' @export
#'
ca.coef <- function(x) {
  if(class(x) != 'ca') stop("The x must be a ca object")
  res <- summary(x)
  row.coef <- cbind(res$rows[, 5], res$rows[, 8])/1000
  rownames(row.coef) <- x$rownames
  col.coef <- cbind(res$columns[,5], res$columns[, 8])/1000
  rownames(col.coef) <- x$colnames
  colnames(row.coef) <- colnames(col.coef) <- c('Dim 1', 'Dim 2')
  list(row.coef=row.coef, col.coef=col.coef)
}
