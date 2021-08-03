#' Create plots to assess predictive performance following cross-validation
#' 
#' @param x An object of class cv.
#' @param ... Further arguments are ignored.
#' @method plot cv
#' @importFrom classifierplots classifierplots
#' @seealso [crossvalidate()]
#' @export
plot.cv <- function(x, ...) {
  classifierplots(as.factor(x$pooled[[3]]), x$pooled$prediction)
}