#' Pool AUC estimates from multiply imputed data using Rubin's rules
#'
#' @param roc An object of class [roc]
#' @param n Sample size
#'
#' @return A list with elements *estimate* and *ci* providing the pooled AUC
#' and its 95% confidence interval respectively.
#' @author Peter Humburg
#' @importFrom pROC auc
#' @importFrom pROC var
#' @export
pool_auc <-
function(roc, n) {
  auc_pooled <- pool.scalar(sapply(roc, auc), sapply(roc, var), n)
  auc_pooled_ci <- auc_pooled$qbar + c(-sqrt(auc_pooled$t), sqrt(auc_pooled$t))*1.96
  list(estimate=auc_pooled$qbar, ci=auc_pooled_ci)
}
