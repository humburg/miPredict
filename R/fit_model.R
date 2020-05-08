#' Variable selection and model fitting
#' 
#' Selects variables for predictive models from multiply imputed
#' data. 
#' 
#' @details
#' The variable selection and model fitting process proceeds in three stages.
#' 1. *LASSO* regression is used to select a subset of predictors for each
#'   imputed dataset.
#' 2. A consensus set of predictors is determined as follows
#'     a. The frequency with which each predictor is selected across the
#'       imputed datasets is determined. 
#'     b. Starting with the most frequently selected predictors, create a series
#'       of models that include increasing numbers of predictors.
#'     c. Select the model with the lowest AIC.
#' 3. The consensus model parameters are estimated on all imputed datasets without penalty.
#' 4. Model parameters are pooled to obtain the final model.
#' 
#' The pooled model returned by this function is useful to generate predictions from new data.
#' If you are interested in inspecting the parameter estimates and their variances instead use
#' `pool(ans$selected_model$fit)`, where `ans` is the object returned by this function.
#' 
#' @param data An object of class [mids]
#' @param outcome The name of the outcome measure. This should be the name of a variable in `data`.
#' @param family The [family] object to be used. May a family object or the name of the family to be used.
#' @param s A character string indicating which value of \eqn{\lambda} should be used. Either `lambda.min`
#' for the optimal \eqn{\lambda} determined by cross-validation or `lambda.1se` for the optimal \eqn{\lambda + 1} standard error.
#' 
#' @return A list with components
#' * *selected_model*: A list with components *formula* and *fit* containing the [formula] for the selected
#'   model and the model fits on the imputed datasets respectively.
#' * *pooled_model*: An object of class [glm] containing the pooled parameter estimates.
#' 
#' @author Peter Humburg
#' @importFrom pROC roc
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom fmsb NagelkerkeR2
#' @export
fit_model <-
function(data, outcome, family="binomial", s=c("lambda.min", "lambda.1se")) {
  s <- match.arg(s)
  fit <- select_variables(data, outcome, family)
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
