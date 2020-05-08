#' @importFrom glmnetUtils cv.glmnet
#' @importFrom stats as.formula
#' @export
select_variables <-
function(data, response, family="binomial", ...){
  var_names <- names(data)[-(1:2)]
  fit_formula <- as.formula(paste0(response, " ~ ", paste(var_names[var_names != response], collapse="+")))
  by(data, data[[".imp"]], function(x) {
    cv.glmnet(fit_formula, family=family, alpha=1, data=x)
  })
}
