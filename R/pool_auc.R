#' Pool AUC estimates from multiply imputed data using Rubin's rules
#'
#' @param roc A list of objects of class [roc]
#' @param n Sample size
#' @param conf_level Size of confidence interval
#'
#' @return A numeric vector with elements *estimate*, *ci.lower*, and *ci.upper* providing the pooled AUC
#' and its 95% confidence interval respectively.
#' @author Peter Humburg
#' @importFrom pROC auc
#' @importFrom pROC var
#' @importFrom stats qnorm
#' @internal
#' @rdname deprecated
pool_auc <-
function(roc, n, conf_level=0.95) {
  auc_pooled <- pool.scalar(sapply(roc, auc), sapply(roc, var), n)
  se_factor <- qnorm(1-(1-conf_level)/2, sd=sqrt(auc_pooled[["t"]]))
  auc_pooled_ci <- auc_pooled$qbar + c(-sqrt(auc_pooled$t/n), sqrt(auc_pooled$t/n))*se_factor
  c(estimate=auc_pooled$qbar, ci.lower=max(0, auc_pooled_ci[1]), ci.upper=min(1, auc_pooled_ci[2]))
}
