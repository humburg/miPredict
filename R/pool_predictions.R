#' Pool predictions
#' 
#' @param pred A matrix of predicted outcomes. One column per imputed dataset.
#' 
#' @return A vector of pooled predictions.
#' @author Peter Humburg
#' @export
pool_predictions <-
function(pred) {
  w <- rep(1/ncol(pred), ncol(pred))
  colSums(w*t(pred))
}
