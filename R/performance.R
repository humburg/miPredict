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
#' @importFrom purrr reduce
#' @export
#' @include internals.R
performance.binomial <- function(model, data, outcome, metrics=c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision", "brier", "r2", "hoslem"), model_fits, ...){
  metrics <- match.arg(metrics, several.ok = TRUE)
  dots <- list(...)
  data <- data_long(data)
  sample_size <- max(data$.id)
  
  predictions <- predict_outcome(model, data)
  attr(predictions, 'dim') <- NULL
  data$prediction <- predictions
  
  perf <- vector(length=length(metrics), mode="list")
  names(perf) <- metrics
  
  ## compute standard performance metrics on each dataset and pool results
  class_perf_metrics <- intersect(metrics, eval(formals(class_perf)$metrics))
  if(length(class_perf_metrics)) {
    class_perf_args <- list()
    if(length(dots)){
      class_perf_args <- dots[names(dots) %in% names(formals(class_perf))]
    }
    perf_raw <- by(data, data$.imp, 
                   function(x) do.call(class_perf, c(list(model=model, data=x, outcome=outcome, 
                                                          metrics=class_perf_metrics), class_perf_args)),
                   simplify=FALSE)
    perf_comb <- reduce(perf_raw, function(x, y){
      mapply(rbind, x, y, SIMPLIFY=FALSE)
    })
    ci_level <- if("level" %in% names(class_perf_args)) class_perf_args$level else formals(class_perf)$level
    se_factor <- qnorm(1-(1-ci_level)/2)
    perf_var <- lapply(perf_comb, function(x) ((x[,3] - x[,2])/se_factor*sample_size)^2)
    perf_est <- lapply(perf_comb, "[", , 2)
    perf_pooled <- mapply(pool.scalar, perf_est, perf_var, MoreArgs=list(n=sample_size))
    perf_pooled_ci <- apply(perf_pooled, 2, function(x) list(c(estimate=x[["qbar"]], ci.lower=max(0, x[["qbar"]] - se_factor*sqrt(x[["t"]])/sample_size), 
                          ci.upper=min(1, x[["qbar"]] + se_factor*sqrt(x[["t"]])/sample_size))))
    perf[class_perf_metrics] <- perf_pooled_ci
  }
  
  if("roc" %in% metrics) {
    perf[["roc"]] <- by(data, data$.imp, 
                        function(x) roc(x[[outcome]], x$prediction, ci=TRUE, plot=FALSE, 
                                        direction="<", levels=c("0", "1")))
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
      tryCatch({
        d <- list(obs=x[[outcome]], exp=x[["prediction"]])
        hoslem <- do.call(logitgof, c(d, hoslem_args))
        ## clean up data record
        hoslem$data.name <- paste0("data$", outcome, ", prediction")
        hoslem
        },
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