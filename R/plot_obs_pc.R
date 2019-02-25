#' Plot observations in the coordinate plane PC1-PC2.
#'
#' This function plots the observations in the coordinate plane PC1-PC2.
#'
#' @param mod A prcomp or princomp object.
#' @param lchar The number of first characters to identify points.
#' @param pch an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#'
#' @seealso [plot_var_pc()].
#'
#' @examples
#' # For a prcomp object
#' pca1 <- prcomp(USArrests, scale=TRUE)  # Example
#' plot_obs_pc(pca1)                              # Default plot
#' plot_obs_pc(pca1, lchar=3, col='blue', pch=8)  # Customized plot
#' #
#' # For a princomp object
#' pca2 <- princomp(x=USArrests, cor=TRUE)
#' plot_obs_pc(pca2, col='red', ylim=c(-4, 4), xlim=c(-4, 4))
#'
#' @return The function returns a list with two elements, coordinate points for first
#' and second dimensions.
#'
#' @importFrom graphics plot points text abline
#' @export
#'
plot_obs_pc <- function(mod, lchar=3, pch=20, ...) {
  stopifnot(class(mod) %in% c('prcomp', 'princomp'))
  if (class(mod) == 'prcomp') {
    scores <- mod$x
  } else {
    scores <- mod$scores
  }
  if (is.null(rownames(scores))) {
    n <- dim(scores)[1]
    rownames(scores) <- 1:n
    lchar <- nchar(n)
  }
  Names <- substr(rownames(scores), start=1, stop=lchar)
  plot(scores[, 1:2], type='n', las=1, ...)
  points(scores[, 1:2], pch=pch, ...)
  text(scores[, 1:2], labels=Names, adj=c(0, 0))
  abline(h=0, col="gray60", lty='longdash')
  abline(v=0, col="gray60", lty='longdash')
}
