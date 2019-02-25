#' Plot variables in the coordinate plane PC1-PC2.
#'
#' This function plots the variables in the coordinate plane PC1-PC2.
#'
#' @param mod A prcomp or princomp object.
#' @param col.arrow color for the arrows.
#' @param xlab,ylab labels for x axis and y axis.
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#'
#' @seealso [plot_obs_pc()].
#'
#' @examples
#' # For a prcomp object
#' pca1 <- prcomp(USArrests, scale = TRUE)  # Example
#' plot_var_pc(pca1)
#' plot_var_pc(pca1, main='Variables in PC1 and PC2',
#'            col='blue3', lwd=3)
#' #
#' # For a princomp object
#' pca2 <- princomp(x=USArrests, cor=TRUE)
#' plot_var_pc(pca2, col='red', lwd=4)
#'
#' @importFrom graphics abline arrows text plot
#' @export
#'
plot_var_pc <- function(mod, col.arrow='gray60', xlab=NULL, ylab=NULL, ...) {
  stopifnot(class(mod) %in% c('prcomp', 'princomp'))
  if (class(mod) == 'prcomp') {
    x <- mod$rotation[, 1:2]
  } else {
    x <- mod$loadings[, 1:2]
  }
  if (is.null(xlab)) xlab <- "PC1"
  if (is.null(ylab)) ylab <- "PC2"
  n <- nrow(x)
  plot(NULL, xlim=c(-1, 1), ylim=c(-1, 1),
       xlab=xlab, ylab=ylab, las=1, ...)
  abline(h=0, col="gray30", lty='longdash')
  abline(v=0, col="gray30", lty='longdash')
  for (i in 1:n) arrows(0, 0, x[i, 1], x[i, 2], col=col.arrow, ...)
  text(x[, 1:2], cex=1, labels=rownames(x), adj=c(0, 0))
}
