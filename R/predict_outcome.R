#' @import stats
#' @export
predict_outcome <-
function(fit, data, ...) {
  stopifnot(".imp" %in% names(data))
  pred <- by(data, data$.imp, function(x) {
    predict(fit, newdata=x, type="response", ...)
  })
  do.call(cbind, pred)
}
