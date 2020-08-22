#' Pooling Hosmer-Lemeshow Goodness of Fit Statistic
#'
#' @param pooled_model Model with pooled parameter estimates.
#' @param data Multiply imputed data. Either an object of class [mids] or a data frame as produced by [mids::complete()].
#' @param outcome Name of the column containing the outcome variable.
#' @param prediction Model predictions as a single vector.
#' @param iter Number of iterations for bootstrapping.
#' @param ... Further arguments to be passed to [hoslem].
#'
#' @return A vector with the pooled 
#' @export
pool_hoslem <- function(pooled_model, data, outcome, prediction, iter=1000, ...){
  data <- data_long(data)
  tests <- hoslem(data, outcome, prediction, ...)
  obs <- sapply(tests, "[[", "statistic")
  df <- sapply(tests, "[[", "parameter")
  df <- df[!is.na(df)][1]
  boot <- vector(mode="list", length=length(obs))
  boot_hoslem <- matrix(ncol = iter, nrow = length(obs))
  for(i in 1:length(obs)){
    boot[[i]] <- lapply(1:iter, function(k) {
      d <- filter(data, .data$.imp==i)
      idx <- sample(1:nrow(d), size=nrow(d), replace=TRUE)
      d[idx,]
    })
  }
  for(j in 1:iter) {
    d <- do.call(rbind, lapply(boot, "[[", j))
    if(missing(prediction)){
      prediction <- predict(pooled_model, d)
    }
    boot_hoslem[,j] <- sapply(hoslem(d, outcome, prediction=prediction, ...), "[[", "statistic")
  }
  vars <- apply(boot_hoslem, 1, sd)^2
  pooled <- pool.scalar(obs, vars)
  c(hoslem=pooled$qbar, sd=sqrt(pooled$t), p.value=pchisq(pooled$qbar, df=df, lower.tail=FALSE)) 
}