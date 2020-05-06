#' @import mice
#' @export
pool_model <-
function(model, data) {
  estimates <- pool(model$fit)
  pooled_coefs <- estimates$pooled[-1, "estimate"]
  names(pooled_coefs) <- estimates$pooled[-1, "term"]
  terms <- list("(Intercept)"=estimates$pooled[1,"estimate"])
  if(length(pooled_coefs)) terms <- c(terms, as.list(pooled_coefs))
  do.call(makeglm, c(list(model$formula, family="binomial", data=data), terms))
}
