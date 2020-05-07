#' @import mice
#' @importFrom pROC auc
#' @importFrom pROC ci
#' @importFrom pROC roc
#' @export
boot_model <- function(imputed, outcome, seed, family="binomial", iter=1000, s="lambda.min", ...){
  ans <- vector(mode="list", length=iter)
  if(!missing(seed)) set.seed(seed)
  imputed_complete <- complete(imputed)
  for(i in 1:ter){
    if(i %% 100 == 0) message(format(Sys.time(), format="%H:%M:%S"), " - Iteration ", i, "/", iter)
    sample_idx <- sample(1:nrow(imputed$data), replace=TRUE)
    imp <- mice(data=imputed$data[sample_idx, ], m=imputed$m, method=imputed$method, ...)
    data_comp <- mice::complete(imp, action="long") %>% clean_data()
    model <- fit_model(data_comp, outcome, family = family, s=s)
    pooled_orig <- pool_predictions(predict_outcome(model$pooled_model, imputed))
    
    imputed$prediction <- pooled_orig
    roc_data <- by(imputed_complete, imputed$.imp, 
                   function(x) roc(x[[outcome]], x$prediction, ci=TRUE, plot=FALSE, 
                                         direction="<", levels=c("0", "1")))
    auc <- sapply(roc_data, auc)
    auc_ci <- sapply(roc_data, ci)
    brier_orig <- tapply(imputed[[outcome]], imputed$.imp,
                         function(x) mean((x - pooled_orig)^2))
    
    ans[[i]] <- c(model, list(data=sample_idx, orig.roc=roc_data, orig.auc=auc, orig.auc_ci=auc_ci, orig.brier=brier_orig))
  }
  ans
}