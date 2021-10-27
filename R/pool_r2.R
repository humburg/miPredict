#' Pool estimates of R-squared
#'
#' @param pooled_model The pooled model returned by [fit_model()].
#' @param model_fits List of model objects as returned by [fit_model()].
#' @param data Imputed data
#' @param ... Further arguments to be passed to specific methods
#'
#' @return A numeric vector with the pooled estimate of $R^2$ and its variance.
#' @seealso [pool_r2.binomial()], [pool_r2.gaussian()]
#' @export
#' @rdname pool_r2
pool_r2 <- function(model, data, ...){
  UseMethod("pool_r2")
}

#' @importFrom rsq rsq
#' @importFrom mice pool.scalar
pool_r2_boot <- function(pooled_model, model_fits, data, type, iter=1000, ...) {
  data <- data_long(data)
  obs <- unlist(sapply(model_fits, rsq, type=type))
  vars <- numeric(length(unique(data$.imp)))
  for(i in 1:length(vars)){
    boot <- lapply(1:iter, function(k) boot_fit(pooled_model, filter(data, .data$.imp==i)))
    boot_r2 <- unlist(sapply(boot, rsq, type=type))
    vars[i] <- sd(boot_r2)^2
  }
  pooled <- pool.scalar(obs, vars)
  c(R2=pooled$qbar, sd=sqrt(pooled$t))
}

#' Obtain pooled estimates of Nagelkerke's R-squared
#'
#' @inheritParams pool_r2
#' @inheritDotParams pool_r2
#' @param iter Number of bootstrap iterations
#'
#' @return A numeric vector with the pooled estimate of $R^2$ and its variance.
#' @method pool_r2 binomial
#' @export
pool_r2.binomial <- function(pooled_model, model_fits, data, iter=1000, ...) {
  pool_r2_boot(pooled_model, model_fits, data, iter=1000, type="n")
}

#' Obtain pooled estimates of R-squared
#'
#' @inheritParams pool_r2
#' @inheritDotParams pool_r2
#' @param iter Number of bootstrap iterations
#'
#' @return A numeric vector with the pooled estimate of $R^2$ and its variance.
#' @importFrom dplyr select
#' @method pool_r2 gaussian
#' @export
pool_r2.gaussian <- function(pooled_model, model_fits, data, iter=1000, ...){
  pool_r2_boot(pooled_model, model_fits, data, iter=1000, type="v")
}