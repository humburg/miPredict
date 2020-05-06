#' @export
pool_predictions <-
function(pred) {
  w <- rep(1/ncol(pred), ncol(pred))
  colSums(w*t(pred))
}
