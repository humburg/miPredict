#' Bootstrap model fit and predictions
#' 
#' @param imputed An object of class [mids] containing the imputed data.
#' @param outcome A character string identifying the outcome measure.
#' @param seed The random seed to use.
#' @param family The [family] to be used for modelling the data. Assuming the use of logistic regression by default.
#' @param iter Number of bootstrap iterations to run.
#' @param s A character string indicating which value of \eqn{\lambda} should be used. Either `lambda.min`
#' for the optimal \eqn{\lambda} determined by cross-validation or `lambda.1se` for the optimal \eqn{\lambda + 1} standard error.
#' @param do_impute Logical indicating whether imputation of missing values should be performed as part of the bootstrap procedure.
#' @param ... Further arguments passed to [performance].
#'
#' @details
#' When `do_impute` is `TRUE` (the default), observations are sampled from the original dataset, including missing values.
#' These data as well as all parameters for the imputation of bootstrapped data are taken from `imputed` argument. When
#' `do_impute` is `FALSE` no new imputation takes place. Instead, resampling of the individuals in the original dataset
#' takes place in such a way that the observations from all imputed datasets are included for a selected individual.
#'
#' @return A list with the results of calls to [fit_model] and [performance] for each bootstrap sample.
#' @author Peter Humburg
#' @import mice
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom pROC roc
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr sample_frac
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
boot_model <- function(imputed, outcome, seed=Sys.time(), family="binomial", iter=1000, s=c("lambda.min", "lambda.1se"), do_impute=TRUE, ...){
  stopifnot(is(imputed, "mids"))
  s <- match.arg(s)
  ans <- vector(mode="list", length=iter)
  set.seed(seed)
  imputed_complete <- data_long(imputed)
  if(explicit_na <- is.factor(imputed$data[[outcome]])){
    imputed$data <- imputed$data %>% mutate(!!outcome := forcats::fct_explicit_na(.data[[outcome]]))
  }
  for(i in 1:iter){
    if(i %% max(round(iter/10), 1) == 0) message(format(Sys.time(), format="%H:%M:%S"), " - Iteration ", i, "/", iter)
    if(do_impute) {
      boot_sample <- imputed$data %>% group_by(.data[[outcome]]) %>% sample_frac(replace=TRUE) %>% ungroup()
      if(explicit_na){
        boot_sample <- boot_sample %>% mutate(!!outcome := forcats::fct_recode(.data[[outcome]], NULL='(Missing)'))
      }
      imp <- mice(data=boot_sample, m=imputed$m, method=imputed$method, printFlag=FALSE)
      data_comp <- data_long(imp)
    } else {
      sample_id <- imputed_complete %>% filter(.data$.imp==1) %>% select(.data$.id, .data[[outcome]]) %>% group_by(.data[[outcome]]) %>% sample_frac(replace=TRUE)
      data_comp <- imputed_complete %>% filter(.data$.id %in% sample_id$.id)
    }
    model <- fit_model(data_comp, outcome, family = family, s=s)
    perf <- performance(model$pooled_model, data_comp, outcome, model_fits=model$selected_model$fit, ...)
    ans[[i]] <- c(model, perf)
  }
  ans
}