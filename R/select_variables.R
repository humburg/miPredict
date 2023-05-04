#' @importFrom glmnetUtils cv.glmnet
#' @importFrom stats as.formula
#' @export
select_variables <-
function(data, response, family="binomial", include, ...){
  var_names <- names(data)[-(1:2)]
  var_names <- var_names[var_names != response]
  fit_formula <- as.formula(paste0(response, " ~ ", paste(var_names, collapse="+")))
  penalty.factor = rep(1, length(var_names))
  if(!missing(include) && !is.null(include) && length(include) > 0) {
    penalty.factor[var_names %in% include] <- 0
  }
  by(data, data[[".imp"]], function(x) {
    cv.glmnet(fit_formula, family=family, alpha=1, data=x, penalty.factor=penalty.factor, ...)
  })
}
