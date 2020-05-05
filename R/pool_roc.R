#' @importFrom pROC coords
#' @importFrom pROC ci.se
#' @import mice
pool_roc <-
function(roc, n) {
  fpr <- sort(unique(unlist(lapply(roc, coords, ret="fpr", drop=TRUE, transpose=FALSE))))
  sens <- sapply(roc, coords, x=1-fpr, input="specificity", ret="sensitivity", drop=TRUE, transpose=FALSE)
  sens_ci <- sapply(roc, function(x) ci.se(x, specificities=1-fpr)[,1])
  sens_se <- (sens-sens_ci)/1.96

  roc_pooled <- apply(sens, 1, pool.scalar, sens_se^2, n)
  sens_pooled <- sapply(roc_pooled, "[[", "qbar")
  sens_pooled_se <- sqrt(sapply(roc_pooled, "[[", "t"))
  data.frame(fpr=fpr, sensitivity=sens_pooled, lower=pmax(sens_pooled-1.96*sens_pooled_se,0), upper=pmin(sens_pooled+1.96*sens_pooled_se, 1))
}
