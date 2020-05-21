#' Pool AUC estimates from multiply imputed data using Rubin's rules
#'
#' @param roc A list of objects of class [roc]
#'
#' @return A list with elements *estimate* and *ci* providing the pooled AUC
#' and its 95% confidence interval respectively.
#' @author Peter Humburg
#' @importFrom pROC auc
#' @importFrom pROC var
#' @export
pool_auc <-
function(roc) {
  n <- length(roc[[1]]$cases) + length(roc[[1]]$controls)
  auc_pooled <- pool.scalar(sapply(roc, auc), sapply(roc, var), n)
  est <- min(max(auc_pooled$qbar, 0), 1)
  auc_pooled_ci <- est + c(-sqrt(auc_pooled$t), sqrt(auc_pooled$t))*1.96
  auc_pooled_ci <- pmin(pmax(auc_pooled_ci, 0), 1)
  list(estimate=est, ci=auc_pooled_ci)
}
