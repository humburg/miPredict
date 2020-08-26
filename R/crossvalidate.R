#' Cross-validation
#'
#' @param imputed An object of class [mice::mids] containing the multiply imputed dataset.
#' @param outcome Name of the outcome variable in `imputed`.
#' @param k Number of folds to use for cross-validation.
#' @param force Logical indicating whether cross-validation should be carried out even if the number of observations becomes too small in some of the folds.
#' @param ... Further arguments passed to [fit_model()].
#'
#' @details The original dataset is partitioned into *k* segments for cross-validation in such a way that
#' the proportion of each outcome, including missing values, are preserved as far as possible.
#' 
#' @return A vector of predictions.
#' @importFrom rlang sym
#' @importFrom rlang !!
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @export
crossvalidate <- function(imputed, outcome, k=10, force=FALSE, ...){
  stopifnot(is(imputed, "mids"))
  loo <- FALSE
  if(k == nrow(imputed$data)){
    loo <- TRUE
    message("Running leave-one-out cross-validation")
  }
  else message("Running ", k, "-fold cross-validation.\n  average number of observations per fold: ", nrow(imputed$data)/k)
  pred <- numeric(nrow(imputed$data)) + NA
  if(!is.factor(imputed$data[[outcome]])){
    imputed$data <- imputed$data %>% mutate(!!outcome := factor(.data[[outcome]]))
  }
  imputed$data <- imputed$data %>% mutate(!!outcome := forcats::fct_explicit_na(.data[[outcome]]))
  if(loo){
   data_orig <- imputed$data %>% mutate(fold=1:n())
  } else {
    data_orig <- imputed$data %>% group_by(!!sym(outcome)) %>% mutate(fold=sample(rep(1:k, length.out=n()), size = n(), replace=FALSE)) %>% ungroup()
  }
  
  ## check all classes are present in all training sets
  test_composition <- table(data_orig[[outcome]], data_orig[["fold"]])
  train_composition <- apply(test_composition, 2, function(cmp) table(data_orig[[outcome]]) - cmp)
  if(any(train_composition == 0)){
    missing <- which(train_composition == 0, arr.ind = TRUE)
    missing <- missing[order(rownames(missing)), 2]
    warn <- which(rownames(missing) == "(Missing)")
    if(length(warn)) {
      msg <- paste("The following training sets contain no missing values:", paste(missing[warn], collapse=", "))
      warning(warn)
    }
  }
  if(any(train_composition < 8)) {
    missing <- which(train_composition < 8, arr.ind = TRUE)
    missing <- missing[order(rownames(missing)), 2]
    msg <- which(rownames(missing) != "(Missing)")
    msg <- paste(paste("Too few observations of class ", rownames(msg), "in training set ", msg), collapse="\n  ")
    msg <- paste0("The following errors occured while partitioning the data:\n", "  ", msg, "\n")
    if(force){
      warning(msg)
    } else {
      stop(msg)
    }
  }
  
  for(i in 1:k){
    data_smpl <- data_orig %>% filter(fold != i) %>%  select(-fold)
    data_smpl <- data_smpl %>% mutate(!!outcome := forcats::fct_recode(.data[[outcome]], NULL='(Missing)'))
    data_imp <- mice(data=data_smpl, m=imputed$m, method=imputed$method, printFlag=FALSE)
    fit <- fit_model(data_imp, outcome=outcome, ...)
    pred[data_orig$fold == i] <- predict(fit$pooled_model, newdata=filter(data_orig, fold==i), type="response")
  }
  pred
}