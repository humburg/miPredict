#' Assessing model performance
#' 
#' Compute model performance metrics.
#'
#' @param model An object of class [glm].
#' @param data Data to use in performance assessment.
#' @param outcome Name of outcome in `data`.
#' @param which Character vector indicating which performance measures should be computed. One or more of *auc*, *specificity*, *sensitivity*, *accuracy*, *precision*.
#' @param bootstrap Logical indicating whether bootstrap samples should be generated from `data`.
#' @param iter Number of bootstrap iterations to use.
#' @param level Width of the confidence interval.
#' 
#' @details Confidence intervals may be calculated for a range of performance metrics, including the AUC, specificity, sensitivity, accuracy, and precision. Of these all except AUC
#' depend on the choice of classification cut-off used. The metrics reported by this function are at the cut-off which maximises the sum of sensitivity and specificity,
#' i.e. the Youden Index.
#' 
#' @return A list of performance measures of the same length as `which`.
#' @export
#' @author Peter Humburg
#'
test_fit <- function(model, data, outcome, which=c("auc", "specificity", "sensitivity", "accuracy", "precision"), bootstrap=FALSE, iter=2000, level=0.95){
  
}