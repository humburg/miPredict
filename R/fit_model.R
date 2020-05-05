#' @importFrom pROC roc
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom fmsb NagelkerkeR2
#' @export
fit_model <-
function(data, outcome, s="lambda.min") {
  fit <- select_variables(data, outcome)
  cand_models <- candidate_models(fit, s=s, response = outcome)
  model <- select_model(cand_models, data)
  pooled_model <- pool_model(model, data)
  predictions <- predict_outcome(pooled_model, data)
  pooled_predictions <- pool_predictions(predictions)
  data$prediction <- pooled_predictions
  
  roc_data <- by(data, data$.imp, 
                 function(x) roc(x[[outcome]], x$prediction, ci=TRUE, plot=FALSE, 
                                       direction="<", levels=c("0", "1")))
  auc <- sapply(roc_data, auc)
  auc_ci <- sapply(roc_data, ci)
  brier_orig <- by(data, data$.imp,
                       function(x) mean((x[[outcome]] - x$prediction)^2))
  r2 <- sapply(model$fit, NagelkerkeR2)
  r2_pooled <- mean(unlist(r2[2,]))
  
  list(selected_model=model, pooled_model=pooled_model, pooled_predictions=pooled_predictions, roc=roc_data, auc=auc, auc_ci=auc_ci,
       brier=brier_orig, r2_pooled=r2_pooled)
}
