#' @importFrom methods is
data_long <- function(data) {
  if(is(data, "mids")){
    data <- mice::complete(data, action="long") %>% clean_data()
  }
  if(!".imp" %in% names(data)) {
    stop("Expected data frame of complete data exported from `mice`. Use `mice::complete(data, action='long')` to obtain data in the right format after imputation.")
  }
  data
}

#' @importFrom stringr str_starts
which_scale <- function(data) {
  to_scale <- sapply(data, function(x) if(length(unique(x)) > 2 && !is.factor(x)) TRUE else FALSE)
  which(to_scale & !str_starts(names(data), fixed(".")))
}

#' @importFrom stats sd
needs_scaling <- function(data) {
  to_scale <- which_scale(data)
  means <- sapply(data[, to_scale], mean)
  sds <- sapply(data[, to_scale], sd)
  any(abs(means) > 0.5) || any(abs(sds - 1) > 0.5)
}