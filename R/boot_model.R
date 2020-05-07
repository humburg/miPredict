#' @import mice
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom pROC roc
#' @importFrom dplyr select
#' @export
boot_model <- function(imputed, outcome, seed, family="binomial", iter=1000, s="lambda.min", do_impute=TRUE, ...){
  ans <- vector(mode="list", length=iter)
  if(!missing(seed)) set.seed(seed)
  imputed_complete <- complete(imputed, action="long") %>% clean_data()
  for(i in 1:iter){
    if(i %% round(iter/10) == 0) message(format(Sys.time(), format="%H:%M:%S"), " - Iteration ", i, "/", iter)
    if(do_impute) {
      sample_idx <- sample(1:nrow(imputed$data), replace=TRUE)
      imp <- mice(data=imputed$data[sample_idx, ], m=imputed$m, method=imputed$method, print=FALSE, ...)
      data_comp <- mice::complete(imp, action="long") %>% clean_data()
    } else {
      sample_idx <- unlist(lapply(sample(unique(imputed_complete$.id), replace=TRUE), function(id) which(imputed_complete$.id == id)))
      data_comp <- imputed_complete[sample_idx, ]
    }
    model <- fit_model(data_comp, outcome, family = family, s=s)
    pooled_orig <- pool_predictions(predict_outcome(model$pooled_model, imputed_complete))
    
    imputed_complete$prediction <- pooled_orig
    roc_data <- by(imputed_complete, imputed_complete$.imp, 
                   function(x) roc(x[[outcome]], x$prediction, ci=TRUE, plot=FALSE, 
                                         direction="<", levels=c("0", "1")))
    auc <- sapply(roc_data, auc)
    auc_ci <- sapply(roc_data, ci)
    brier_orig <- tapply(imputed_complete[[outcome]], imputed_complete$.imp,
                         function(x) mean((x - pooled_orig)^2))
    
    ans[[i]] <- c(model, list(data=sample_idx, orig.roc=roc_data, orig.auc=auc, orig.auc_ci=auc_ci, orig.brier=brier_orig))
    imputed_complete <- imputed_complete %>% select(-prediction)
  }
  ans
}