#' @import mice
#' @import stats
#' @importFrom tidyr %>%
#' @export
select_model <-
function(candidates, data, family=binomial, ...) {
  ans <- vector(mode="list", length=length(candidates))
  for(i in 1:length(candidates)) {
    ans[[i]] <- by(data, data$.imp, function(x) {
      x <- x %>% scale_data()
      glm(candidates[[i]], data=x, family=family, ...)
    })
  }
  aic <- sapply(lapply(ans, function(x) summary(as.mira(x), type="glance")), "[[", "AIC")
  aic <- colMeans(aic)
  list(formula=candidates[[which.min(aic)]], fit=ans[[which.min(aic)]])
}
