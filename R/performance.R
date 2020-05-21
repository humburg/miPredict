#' Performance metrics for predictive models
#'
#' @param model The (pooled) model to be evaluated.
#' @param data Data to be used in the evaluation.
#' @param ... Further arguments to be passed to specific methods
#'
#' @return A list of performance metrics.
#' @export
#' @rdname performance
performance <- function(model, data, outcome, ...){
  UseMethod("performance")
}


#' 
#' @param metrics A character vector indicating the metrics to be computed
#' @param outcome The name of the outcome measure. This should be the name of a variable in `data`.
#' @param model_fits A list of fitted model objects. Should contain one model for each imputed dataset (required if 'r2' is requested).
#'
#' @rdname performance
#' @method performance binomial
#' @importFrom pROC roc
#' @importFrom pROC ci
#' @importFrom fmsb NagelkerkeR2
#' @importFrom generalhoslem logitgof
#' @export
#' @include internals.R
performance.binomial <- function(model, data, outcome, metrics=c("roc", "auc", "brier", "r2", "hoslem"), model_fits, ...){
  metrics <- match.arg(metrics, several.ok = TRUE)
  dots <- list(...)
  data <- data_long(data)
  
  predictions <- predict_outcome(model, data)
  #pooled_predictions <- pool_predictions(predictions)
  #data$prediction <- pooled_predictions
  attr(predictions, 'dim') <- NULL
  data$prediction <- predictions
  
  perf <- vector(length=length(metrics), mode="list")
  names(perf) <- metrics
  
  if("roc" %in% metrics || "auc" %in% metrics){
    roc_data <- by(data, data$.imp, 
                   function(x) roc(x[[outcome]], x$prediction, ci=TRUE, plot=FALSE, 
                                   direction="<", levels=c("0", "1")))
  }
  if("roc" %in% metrics) {
    perf[["roc"]] <- roc_data
  }
  if("auc" %in% metrics) {
    auc_ci <- t(sapply(roc_data, ci))
    colnames(auc_ci) <- c("AUC", "CI.lower", "ci.upper")
    perf[["auc"]] <- auc_ci
  }
  if("brier" %in% metrics) {
    perf[["brier"]] <- unlist(as.list(by(data, data$.imp,
                     function(x) mean((x[[outcome]] - x$prediction)^2))))
    
  }
  if("r2" %in% metrics){
    if(!missing(model_fits)) {
      perf[["r2"]] <- unlist(sapply(model_fits, NagelkerkeR2)[2,])
    } else{
      warning("Argument 'model_fits' is missing, skipping computation of Nagelkerke's R2.")
    }
  }
  if("hoslem" %in% metrics){
    perf[["hoslem"]] <- unclass(by(data, data$.imp, function(x) {
      if(length(dots)) hoslem_args <- dots[names(dots) %in% names(formals(logitgof))]
      else hoslem_args <- list()
      tryCatch(
        do.call(logitgof, c(list(obs=x[[outcome]], exp=x[["prediction"]]), hoslem_args)),
        error=function(e){
          warning("Hosmer-Lemeshow Test failed with error message '", e, "'")
          ans <- list(statistic=c("X-squared"=NA), parameter=c(df=NA), p.value=NA, method="Hosmer and Lemeshow test", data.name=NA, observed=NA, expected=NA, stddiffs=NA)
          class(ans) <- "htest"
          ans
        }
      )}))
    attr(perf[["hoslem"]], "call") <- NULL
  }
  perf
}