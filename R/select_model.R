#' @import mice
#' @importFrom stats glm
#' @importFrom stats binomial
#' @importFrom tidyr %>%
#' @export
select_model <-
function(candidates, data, family=binomial, ...) {
  ans <- vector(mode="list", length=length(candidates))
  for(i in 1:length(candidates)) {
    ans[[i]] <- by(data, data$.imp, function(x) {
      x <- x %>% scale_data()
      fit <- glm(candidates[[i]], data=x, family=family, ...)
      fit$call$family <- family
      fit$call$formula <- candidates[[i]]
      attr(fit$call$formula, ".Environment") <- as.environment(x)
      fit
    })
  }
  aic <- sapply(lapply(ans, function(m) summary(as.mira(m), type="glance")), "[[", "AIC")
  aic <- colMeans(aic)
  list(formula=candidates[[which.min(aic)]], fit=ans[[which.min(aic)]])
}
