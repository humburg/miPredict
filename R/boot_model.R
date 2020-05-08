#' @import mice
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom pROC roc
#' @export
boot_model <- function(imputed, outcome, seed, family="binomial", iter=1000, s="lambda.min", do_impute=TRUE, ...){
  ans <- vector(mode="list", length=iter)
  if(!missing(seed)) set.seed(seed)
  imputed_complete <- complete(imputed, action="long") %>% clean_data()
  for(i in 1:iter){
    if(i %% round(iter/10) == 0) message(format(Sys.time(), format="%H:%M:%S"), " - Iteration ", i, "/", iter)
    if(do_impute) {
      sample_idx <- sample(1:nrow(imputed$data), replace=TRUE)
      imp <- mice(data=imputed$data[sample_idx, ], m=imputed$m, method=imputed$method, printFlag=FALSE, ...)
      data_comp <- mice::complete(imp, action="long") %>% clean_data()
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