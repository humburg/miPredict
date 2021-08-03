#' @importFrom methods is
data_long <- function(data, include=FALSE, clean=TRUE) {
  if(is(data, "mids")){
    data <- mice::complete(data, action="long", include=include)
  }
  if(!".imp" %in% names(data)) {
    stop("Expected data frame of complete data exported from `mice`. Use `mice::complete(data, action='long')` to obtain data in the right format after imputation.")
  }
  if(clean){
    data <- clean_data(data)
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

perf_var <- function(p, n) {
  p*(1-p)/n
}


#' @importFrom stats formula
#' @importFrom stats family
boot_fit <- function(model, data) {
  idx <- sample(1:nrow(data), nrow(data), replace=TRUE)
  newdata <- data[idx,]
  glm(formula(model), family=family(model), data=newdata)
}

#' @importFrom stats formula
#' @importFrom stats model.matrix
get_formula <- function(predictors, outcome, data) {
  formula <- paste0(outcome, "~", paste(predictors, collapse="+"))
  predictors <- colnames(model.matrix(stats::formula(formula), data=data %>% data_long(clean=FALSE)))[-1] %>% make.names()
  paste0(outcome, "~", paste(predictors, collapse="+"))
}

# Dummy code factors in a mids object
#' @importFrom methods as
expand_factors <- function(data) {
  data %>% data_long(include=TRUE) %>% clean_data() %>% as("mids")
}