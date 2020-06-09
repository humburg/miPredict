#' Assessing model performance
#'
#' @param model An object of class [glm].
#' @param data Data to use in performance assessment.
#' @param which Character vector indicating which performance measures should be computed. One or more of *auc*, *thresholds*, *sp*, *se*, *coords*.
#' @param bootstrap Logical indicating whether bootstrap samples should be generated from `data`.
#' @param iter Number of bootstrap iterations to use.
#' @param level Width of the confidence interval.
#' 
#' @details 
#' Confidence intervals may be calculated for the AUC (with `which="auc"`), thresholds derived from the ROC curve of the `model` on the provided `data` (`which="thresholds"`),
#' specificity (`which="sp"`), sensitivity (`which="se"`), and the ROC curve (`which="coords"`).
#' 
#' @return A list of performance measures of the same length as `which`.
#' @export
#' @author Peter Humburg
#'
#' @examples
test_fit <- function(model, data, which=c("auc", "thresholds", "sp", "se", "coords"), bootstrap=FALSE, iter=2000, level=0.95){
  
}