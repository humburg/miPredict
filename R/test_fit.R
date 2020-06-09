#' Assessing model performance
#' 
#' Compute model performance metrics.
#'
#' @param model An object of class [glm].
#' @param data Data to use in performance assessment.
#' @param outcome Name of outcome in `data`.
#' @param metrics Character vector indicating which performance measures should be computed. One or more of *auc*, *specificity*, *sensitivity*, *accuracy*, *precision*.
#' @param bootstrap Logical indicating whether bootstrap samples should be generated from `data`.
#' @param iter Number of bootstrap iterations to use.
#' @param level Width of the confidence interval.
#' 
#' @details Confidence intervals may be calculated for a range of performance metrics, including the AUC, specificity, sensitivity, accuracy, and precision. Of these all except AUC
#' depend on the choice of classification cut-off used. The metrics reported by this function are at the cut-off which maximises the sum of sensitivity and specificity,
#' i.e. the Youden Index.
#' 
#' @return A list of performance measures of the same length as `metrics`.
#' @export
#' @author Peter Humburg
#'
test_fit <- function(model, data, outcome, metrics=c("auc", "specificity", "sensitivity", "accuracy", "precision"), bootstrap=FALSE, iter=2000, level=0.95){
  metrics <- match.arg(metrics, several.ok = TRUE)
  roc <- pROC::roc(data[[outcome]], predict(model, data))
  ans <- vector(mode="list", length(metrics))
  names(ans) <- metrics
  if("auc" %in% metrics) {
    ans$auc <- pROC::ci.auc(roc, conf.level=level, method=if(bootstrap) "bootstrap" else "delong", boot.n=iter)
    metrics <- metrics[-which(metrics == "auc")]
  }
  if(length(metrics) > 0) {
    ans[metrics] <- ci(roc, "best", of="coords", ret=metrics, conf.level=level, boot.n=iter)[metrics]
  }
  ans
}