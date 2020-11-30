#' Predict outcomes for multiply imputed data
#' 
#' @param fit A fitted model object to use for predictions.
#' @param data A multiply imputed dataset. Either of class [mids] or in *long* format.
#' @param ... Further arguments passed to [predict].
#' 
#' @return A matrix with one column for each imputed dataset.
#' @author Peter Humburg
#' @importFrom stats predict
#' @export
predict_outcome <-
function(fit, data, ...) {
  data <- data_long(data) %>% clean_data()
  if(!is.null(attr(fit, "scale")) && attr(fit, "scale")) {
    data <- data %>% scale_data()
  }
  pred <- by(data, data$.imp, function(x) {
    predict(fit, newdata=x, type="response", ...)
  })
  do.call(cbind, pred)
}
