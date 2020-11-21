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
#' @param scale Logical indicating whether input data should be scaled.
#' @param predictors A character vector with the names of variables to include as predictors. If this is provided the model is fixed and no variable selection will take place.
#' 
#' @return A list with components
#' * *selected_model*: A list with components *formula* and *fit* containing the [formula] for the selected
#'   model and the model fits on the imputed datasets respectively.
#' * *pooled_model*: An object of class [glm] containing the pooled parameter estimates.
#' 
#' @author Peter Humburg
#' @export
fit_model <-
function(data, outcome, family="binomial", s=c("lambda.min", "lambda.1se"), scale=FALSE, predictors) {
  s <- match.arg(s)
  ## skip variable selection if a set of predictors is provided
  if(!missing(predictors) && !is.null(predictors) && length(predictors) > 0){
    formula <- paste0(outcome, "~", paste(predictors, collapse="+"))
    fit <- with(data, glm(formula(formula), family=family))
    model <- list(formula=stats::formula(formula), fit=fit$analyses)
  } else {
    data <- data_long(data) %>% clean_data()
    if(scale) {
      data <- data %>% scale_data()
    } else if(needs_scaling(data)){
      ## warn if data appears to be unscaled
      warning("Data may require scaling. Consider re-running with `scale = TRUE`.")
    }
    fit <- select_variables(data, outcome, family)
    cand_models <- candidate_models(fit, s=s, response = outcome)
    model <- select_model(cand_models, data, family=family)
  }
  pooled_model <- pool_model(model, data)
  class(pooled_model) <- c(pooled_model$family$family, class(pooled_model))
  list(selected_model=model, pooled_model=pooled_model)
}
