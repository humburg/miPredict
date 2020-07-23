#' Pool (decomposed) Brier scores
#' 
#' @param x List of decomposed Brier scores
#' @param n Sample size
#' @param addTotal Logical indicating whether the total Brier score should be computed (TRUE, the default)
#' or whether only the components should be returned.
#' @return 2*3 matrix with pooled estimates of Brier score components
#' @importFrom mice pool.scalar
#' @export
pool_brier <- function(x, n=Inf, addTotal=TRUE) {
  rel <- sapply(x, "[", 1, 'REL')
  rel_sd <- sapply(x, "[", 2, 'REL')
  res <- sapply(x, "[", 1, 'RES')
  res_sd <- sapply(x, "[", 2, 'RES')
  unc <- sapply(x, "[", 1, 'UNC')
  unc_sd <- sapply(x, "[", 2, 'UNC')
  
  rel_pooled <- pool.scalar(rel, rel_sd^2, n)
  res_pooled <- pool.scalar(res, res_sd^2, n)
  unc_pooled <- pool.scalar(unc, unc_sd^2, n)
  if(addTotal){
    total_pooled <- pool.scalar(rel-res+unc, rel_sd^2+res_sd^2+unc_sd^2)
  }
  
  ans <- matrix(0, nrow = 2, ncol = 3)
  rownames(ans) <- c("component", "component.sd")
  colnames(ans) <- c("Reliability", "Resolution", "Uncertainty")
  
  ans[1, ] <- c(rel_pooled$qbar, res_pooled$qbar, unc_pooled$qbar)
  ans[2, ] <- c(sqrt(rel_pooled$t), sqrt(res_pooled$t), sqrt(unc_pooled$t))
  
  if(addTotal){
    ans <- cbind(Brier=c(total_pooled$qbar, sqrt(total_pooled$t)), ans)
  }
  
  ans
}