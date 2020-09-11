#' Performance metrics for predictive models
#'
#' @param x The model to be evaluated.
#' @param ... Further arguments to be passed to specific methods
#'
#' @return A list of performance metrics.
#' @seealso [performance.binomial()], [performance.cv()]
#' @export
#' @rdname performance
performance <- function(x, ...){
  UseMethod("performance")
}


#' Performance metrics for logistic regression models
#' 
#' @param x The pooled logistic regression model to be evaluated.
#' @param data Data to be used in the evaluation.
#' @param metrics A character vector indicating the metrics to be computed
#' @param outcome The name of the outcome measure. This should be the name of a variable in `data`.
#' @param model_fits A list of fitted model objects. Should contain one model for each imputed dataset (required if 'r2' is requested).
#'
#' @method performance binomial
#' @importFrom pROC roc
#' @importFrom pROC ci
#' @importFrom fmsb NagelkerkeR2
#' @importFrom generalhoslem logitgof
#' @importFrom purrr reduce
#' @importFrom SpecsVerification BrierDecomp
#' @importFrom stats qnorm
#' @export
#' @include internals.R
performance.binomial <- function(x, data, outcome, metrics=c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision", "brier", "r2", "hoslem"), model_fits, ...){
  metrics <- match.arg(metrics, several.ok = TRUE)
  dots <- list(...)
  data <- data_long(data)
  sample_size <- max(data$.id)
  
  predictions <- predict_outcome(x, data)
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
                   function(d) do.call(class_perf, c(list(predictions=predict(x, newdata=d, type="response"), outcome=d[[outcome]], 
                                                          metrics=class_perf_metrics), class_perf_args)),
                   simplify=FALSE)
    perf_comb <- reduce(perf_raw, function(x, y){
      mapply(rbind, x, y, SIMPLIFY=FALSE)
    })
    ci_level <- if("level" %in% names(class_perf_args)) class_perf_args$level else formals(class_perf)$level
    se_factor <- qnorm(1-(1-ci_level)/2)
    perf_size <- cbind(sensitivity=unlist(by(data, data$.imp, function(d) sum(d[[outcome]]))), 
                   specificity=unlist(by(data, data$.imp, function(d) sum(!d[[outcome]]))),
                   accuracy=sample_size)
    perf_var <- lapply(perf_comb, function(p) perf_var(p[,2], sample_size))
    perf_est <- lapply(perf_comb, "[", , 2)
    perf_pooled <- mapply(pool.scalar, perf_est, perf_var, MoreArgs=list(n=sample_size))
    perf_pooled_ci <- apply(perf_pooled, 2, function(p) c(estimate=p[["qbar"]], ci.lower=max(0, p[["qbar"]] - se_factor*sqrt(p[["t"]]/sample_size)), 
                          ci.upper=min(1, p[["qbar"]] + se_factor*sqrt(p[["t"]]/sample_size))))
    for(i in 1:length(class_perf_metrics)){
      perf[[class_perf_metrics[i]]] <- perf_pooled_ci[,i]
    }
    
  }
  
  if("roc" %in% metrics) {
    perf[["roc"]] <- by(data, data$.imp, 
                        function(d) roc(d[[outcome]], d$prediction, ci=TRUE, plot=FALSE, 
                                        direction="<", levels=c("0", "1")))
  }
  if("brier" %in% metrics) {
    brier <- as.list(by(data, data$.imp,
                        function(d) BrierDecomp((d$prediction), d[[outcome]])))
    perf[["brier"]] <- pool_brier(brier, sample_size)
    
  }
  if("r2" %in% metrics){
    if(!missing(model_fits)) {
      perf[["r2"]] <- pool_r2(x, model_fits, data)
      
    } else{
      warning("Argument 'model_fits' is missing, skipping computation of Nagelkerke's R2.")
    }
  }
  if("hoslem" %in% metrics){
    if(length(dots)) hoslem_args <- dots[names(dots) %in% names(formals(logitgof))]
    else hoslem_args <- list()
    args <- c(list(x, data, outcome), hoslem_args)
    perf[["hoslem"]] <- 
      tryCatch(do.call(pool_hoslem, args),
        error=function(e){
          warning("Hosmer-Lemeshow Test failed with error message '", e, "'")
          ans <- list(statistic=c("X-squared"=NA), parameter=c(df=NA), p.value=NA, method="Hosmer and Lemeshow test", data.name=NA, observed=NA, expected=NA, stddiffs=NA)
          class(ans) <- "htest"
          ans
        })
    attr(perf[["hoslem"]], "call") <- NULL
  }
  
  perf
}

  perf
}