#' Obtain pooled estimates of Nagelkerke's R-squared
#'
#' @param pooled_model The pooled model returned by [fit_model].
#' @param model_fits List of model objects as returned by [fit_model].
#' @param data Imputed dataset
#' @param iter Number of bootstrap iterations
#'
#' @return A numeric vector with the pooled estimate of $R^2$ and its variance.
#' @importFrom dplyr select
#' @importFrom mice pool.scalar
#' @export
pool_r2 <- function(pooled_model, model_fits, data, iter=1000) {
  data <- data_long(data)
  obs <- unlist(sapply(model_fits, NagelkerkeR2)[2,])
  vars <- numeric(length(unique(data$.imp)))
  for(i in 1:length(vars)){
    boot <- lapply(1:iter, function(k) boot_fit(pooled_model, filter(data, .data$.imp==i)))
    boot_r2 <- unlist(sapply(boot, NagelkerkeR2)[2,])
    vars[i] <- sd(boot_r2)^2
  }
  pooled <- pool.scalar(obs, vars)
  c(R2=pooled$qbar, sd=sqrt(pooled$t))
}