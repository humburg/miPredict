#' Obtain pooled estimates of R-squared
#'
#' @param pooled_model The pooled model returned by [fit_model].
#' @param model_fits List of model objects as returned by [fit_model].
#' @param data Imputed dataset
#' @param iter Number of bootstrap iterations (only used for method="nagelkerke")
#' @param method Type of R^2 to compute and pool
#'
#' @return A numeric vector with the pooled estimate of $R^2$ and its variance.
#' @importFrom dplyr select
#' @importFrom mice pool.scalar
#' @export
pool_r2 <- function(pooled_model, model_fits, data, iter=1000, method=c("nagelkerke", "r2", "adj.r2")) {
  method <- match.arg(method)
  data <- data_long(data)
  n <- nrow(subset(data, .imp == 1))

  if(method == "nagelkerke"){
    obs <- unlist(sapply(model_fits, NagelkerkeR2)[2,])
    vars <- numeric(length(unique(data$.imp)))
    for(i in 1:length(vars)){
      boot <- lapply(1:iter, function(k) boot_fit(pooled_model, filter(data, .data$.imp==i)))
      boot_r2 <- unlist(sapply(boot, NagelkerkeR2)[2,])
      vars[i] <- sd(boot_r2)^2
    }
    .pool_r(sqrt(obs), vars, n, "Nagelkerke's R2")
  } else if(method == "r2"){
    r <- sqrt(sapply(model_fits, function(m) 1 - m$deviance/m$null.deviance))
    se <- 1/sqrt(nrow(subset(data, .imp == 1))-3)
    .pool_r(r, se^2, n, "R2")
  } else if(method == "adj.r2"){
    r <- sqrt(sapply(model_fits, function(m) 1 - (m$deviance/m$df.residual)/(m$null.deviance/m$df.null)))
    se <- 1/sqrt(nrow(subset(data, .imp == 1))-3)
    .pool_r(r, se^2, n, "Adj. R2")
  }
}

.pool_r <- function(r, var, n, label) {
  fisher <- fisher.trans(r)
  pooled <- pool.scalar(fisher, rep(var, length(r)), n=n)
  table <- array(c(fisher.backtrans(pooled$qbar)^2, fisher.backtrans(max(pooled$qbar - 1.96*sqrt(pooled$t), 0))^2, min(fisher.backtrans(pooled$qbar + 1.96*sqrt(pooled$t))^2, 1), pooled$fmi), dim = c(1, 4))
  dimnames(table) <- list(label, c("est", "lo 95", "hi 95", "fmi"))
  table
}