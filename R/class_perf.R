#' Assessing model predictive performance
#' 
#' Compute performance metrics for logistic regression models.
#'
#' @param predictions A numeric vector of predicted class probabilities.
#' @param outcome A vector (numeric or factor) of outcome labels.
#' @param metrics Character vector indicating which performance measures should be computed. One or more of *auc*, *specificity*, *sensitivity*, *accuracy*, *precision*.
#' @param bootstrap Logical indicating whether bootstrap samples should be generated from `data`.
#' @param iter Number of bootstrap iterations to use.
#' @param level Width of the confidence interval.
#' 
#' @details 
#' # Available performance metrics:
#' Confidence intervals may be calculated for a range of performance metrics, including the AUC, specificity, sensitivity, accuracy, and precision. Of these all except AUC
#' depend on the choice of classification cut-off used. The metrics reported by this function are at the cut-off which maximises the sum of sensitivity and specificity,
#' i.e. the Youden Index.
#' 
#' # Using multiply imputed data:
#' To compute performance metrics from multiply imputed data, use the [performance()] function instead.
#' 
#' @return A list of performance measures of the same length as `metrics`.
#' @seealso [performance()]
#' @export
#' @importFrom dplyr select
#' @author Peter Humburg
#'
class_perf <- function(predictions, outcome, metrics=c("auc", "specificity", "sensitivity", "accuracy", "precision"), bootstrap=FALSE, iter=2000, level=0.95){
  metrics <- match.arg(metrics, several.ok = TRUE)
  roc <- pROC::roc(outcome, predictions, 
                   direction="<", levels=c("0", "1"))
  ans <- vector(mode="list", length(metrics))
  names(ans) <- metrics
  if("auc" %in% metrics) {
    ans$auc <- pROC::ci.auc(roc, conf.level=level, method=if(bootstrap) "bootstrap" else "delong", boot.n=iter)
    metrics <- metrics[-which(metrics == "auc")]
  }
  if(length(metrics) > 0) {
    ans[metrics] <- ci(roc, "best", of="coords", ret=metrics, conf.level=level, boot.n=iter, best.policy="random")[metrics]
  }
  ans
}