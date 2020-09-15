#' @import mice
#' @export
pool_model <-
function(model, data) {
  estimates <- pool(model$fit)
  pooled_coefs <- estimates$pooled[-1, "estimate"]
  names(pooled_coefs) <- estimates$pooled[-1, "term"]
  terms <- list("(Intercept)"=estimates$pooled[1,"estimate"])
  if(length(pooled_coefs)) terms <- c(terms, as.list(pooled_coefs))
  pooled <- do.call(makeglm, c(list(model$formula, family="binomial", data=data), terms))
  pooled$deviance <- mean(sapply(model$fit, deviance))
  pooled$null.deviance <- mean(sapply(model$fit, function(x) x$null.deviance))
  pooled$aic <- mean(sapply(model$fit, extractAIC)[,2])
  pooled$qr$qr <- Reduce("+", lapply(model$fit, function(x) x$qr$qr))/length(model$fit)
  colnames(pooled$qr$qr) <- names(coef(pooled))
  pooled$qr$rank <- ncol(pooled$qr$qr)
  pooled$qr$qraux <- Reduce("+", lapply(model$fit, function(x) x$qr$qraux))/length(model$fit)
  class(pooled$qr) <- "qr"
  pooled$weights <- Reduce("+", lapply(model$fit, function(x) x$weights))/length(model$fit)
  pooled$linear.predictors <- Reduce("+", lapply(model$fit, function(x) x$linear.predictors))/length(model$fit)
  pooled$df.residual <- model$fit[[1]]$df.residual
  pooled$df.null <- model$fit[[1]]$df.null
  pooled$call <- model$fit[[1]]$call
  pooled$method <- model$fit[[1]]$method
  pooled$control <- model$fit[[1]]$control
  pooled$offset <- model$fit[[1]]$offset
  pooled
}
