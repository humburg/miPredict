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
#'
#' @details
#' When `do_impute` is `TRUE` (the default), observations are sampled from the original dataset, including missing values.
#' These data as well as all parameters for the imputation of bootstrapped data are taken from `imputed` argument. When
#' `do_impute` is `FALSE` no new imputation takes place. Instead, resampling of the individuals in the original dataset
#' takes place in such a way that the observations from all imputed datasets are included for a selected individual.
#'
#' @author Peter Humburg
#' @import mice
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom pROC roc
#' @export
boot_model <- function(imputed, outcome, seed=Sys.time(), family="binomial", iter=1000, s=c("lambda.min", "lambda.1se"), do_impute=TRUE){
  s <- match.arg(s)
  ans <- vector(mode="list", length=iter)
  set.seed(seed)
  imputed_complete <- complete(imputed, action="long") %>% clean_data()
  for(i in 1:iter){
    if(i %% round(iter/10) == 0) message(format(Sys.time(), format="%H:%M:%S"), " - Iteration ", i, "/", iter)
    if(do_impute) {
      sample_idx <- sample(1:nrow(imputed$data), replace=TRUE)
      imp <- mice(data=imputed$data[sample_idx, ], m=imputed$m, method=imputed$method, printFlag=FALSE)
      data_comp <- complete(imp, action="long") %>% clean_data()
    } else {
      sample_idx <- unlist(lapply(sample(unique(imputed_complete$.id), replace=TRUE), function(id) which(imputed_complete$.id == id)))
      data_comp <- imputed_complete[sample_idx, ]
    }
    model <- fit_model(data_comp, outcome, family = family, s=s)
    perf <- performance(model$pooled_model, data_comp, outcome)
    ans[[i]] <- c(model, list(data=sample_idx), perf)
  }
  ans
}