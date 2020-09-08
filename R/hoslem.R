#' Compute Hosmer-Lemeshow Goodness of Fit statistic
#'
#' @param data Multiply imputed dataset. Either an object of class [mids] or a data frame created with [complete()][mice::complete.mids()].
#' @param outcome Name of the outcome variable.
#' @param prediction A vector of predicted values.
#' @param ... Further arguments are passed on to [generalhoslem::logitgof()]
#'
#' @return A list of objects of class `htest` with one entry per dataset.
#' @importFrom stats pchisq
#' @export
hoslem <- function(data, outcome, prediction, ...) {
  data <- data_long(data)
  if(!"prediction" %in% names(data)) {
    if(missing(prediction)) {
      stop("Predicted values are required. You can provide these either via 'prediction' argument or a column with name 'prediction' in 'data'.")
    }
    data$prediction <- prediction
  }
  dots <- list(...)
  if(length(dots)) hoslem_args <- dots[names(dots) %in% names(formals(logitgof))]
  else hoslem_args <- list()
  ans <- unclass(by(data, data$.imp, function(x) {
    tryCatch({
      d <- list(obs=x[[outcome]], exp=x[["prediction"]])
      hoslem <- do.call(logitgof, c(d, hoslem_args))
      ## clean up data record
      hoslem$data.name <- paste0("data$", outcome, ", prediction")
      hoslem
    },
    error=function(e){
      warning("Hosmer-Lemeshow Test failed with error message '", e, "'")
      ans <- list(statistic=c("statistic"=NA), parameter=c(df=NA), p.value=NA, method="Hosmer and Lemeshow test", data.name=NA, observed=NA, expected=NA, stddiffs=NA)
      class(ans) <- "htest"
      ans
    }
    )}))
  attr(ans, "call") <- NULL
  ans
}