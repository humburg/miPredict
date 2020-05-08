#' @importFrom stats predict
#' @export
predict_outcome <-
function(fit, data, ...) {
  data <- data_long(data)
  pred <- by(data, data$.imp, function(x) {
    predict(fit, newdata=x, type="response", ...)
  })
  do.call(cbind, pred)
}
