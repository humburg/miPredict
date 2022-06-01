context("model performance")

test_that("output for binomial models has expected structure", {
  expect_named(suppressWarnings(perf1 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "roc")), "roc")
  expect_equal(class(perf1$roc[[1]]), "roc")
  expect_equal(length(perf1$roc), binom_mids$m)
  
  expect_named(suppressWarnings(perf2 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc"))), c("roc", "auc"))
  expect_equal(class(perf2$auc), "numeric")
  
  expect_named(suppressWarnings(perf2a <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "auc")), "auc")
  expect_equal(names(perf2a), "auc")
  expect_equal(class(perf2a$auc), "numeric")
  expect_equal(length(perf2a$auc), 3)
  
  expect_named(suppressWarnings(perf3 <- performance(binom_fit$pooled_model, binom_mids, "y", 
                                                     metrics = c("roc", "auc", "brier"))), c("roc", "auc", "brier"))
  expect_equal(class(perf3$brier[[1]]), "numeric")
  expect_equal(length(perf3$brier), 8)
  
  expect_named(suppressWarnings(perf4 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2"), 
                                                     binom_fit$selected_model$fit)), c("roc", "auc", "brier", "r2"))
  expect_equal(class(perf4$r2[[1]]), "numeric")
  expect_equal(length(perf4$r2), 2)
  
  expect_named(suppressWarnings(perf5 <- performance(binom_fit$pooled_model, binom_mids, "y", 
                                                     metrics = c("roc", "auc", "brier", "hoslem"))), 
               c("roc", "auc", "brier", "hoslem"))
  expect_type(perf5$hoslem, "double")
  expect_length(perf5$hoslem, 3)
})

test_that("output for Gaussian models has expected structure", {
  fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE)
  expect_named(perf1 <- performance(fit$pooled_model, data=nhanes_mids, outcome = "bmi", metrics="r2", model_fits=fit$selected_model$fit))
  
})

test_that("missing arguments are handled", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2")), "model_fits")
})

test_that("performance metrics fail gracefully", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", "hoslem", g=0), "Test failed")
})

test_that("arguments are passed on", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", "hoslem", g=5), "< 1")
})

test_that("pooled CV performance can be calculated", {
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2))
  set.seed(976)
  expect_named(perf <- performance(cv, call_imputed(nhanes_mids, "hyp")), c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision", "brier"))
  expect_equal(colnames(perf$brier), c("Brier", "Reliability", "Resolution", "Uncertainty"))
  set.seed(976)
  expect_named(perf2 <- performance(cv, "hyp"), c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision", "brier"))
  expect_equal(perf, perf2)
})

test_that("CV performance can be pooled", {
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2))
  expect_named(perf <- performance(cv, call_imputed(nhanes_mids, "hyp")), c("roc", "auc", "specificity", "sensitivity", "accuracy", "precision", "brier"))
  expect_s3_class(perf$roc, "roc")
  expect_s3_class(perf$auc, "ci.auc")
  expect_type(perf$specificity, "double")
  expect_type(perf$sensitivity, "double")
  expect_type(perf$accuracy, "double")
  expect_type(perf$precision, "double")
  expect_type(perf$brier, "double")
})

test_that("performace of fixed models can be calculated", {
  expect_silent(fit_fixed <- fit_model(nhanes_mids, outcome="hyp", predictors="bmi"))
  expect_named(suppressWarnings(perf_fixed <- performance(fit_fixed$pooled_model, nhanes_mids, "hyp", metrics = c("roc", "auc", "brier", "r2"), 
                                                     fit_fixed$selected_model$fit)), c("roc", "auc", "brier", "r2"))
})

test_that("Linear model performance can be calculated", {
  expect_silent(fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE))
  expect_named(suppressWarnings(perf1 <- performance(fit$pooled_model, nhanes_mids, model_fits=fit$selected_model$fit, "bmi", metrics = "r2")), "r2")
  expect_type(perf1, "list")
  expect_type(perf1[[1]], "numeric")
  expect_named(suppressWarnings(perf2 <- performance(fit$pooled_model, nhanes_mids, model_fits=fit$selected_model$fit, "bmi", metrics = "adj.r2")), "adj.r2")
  expect_type(perf2, "list")
  expect_type(perf2[[1]], "numeric")
  expect_named(suppressWarnings(perf3 <- performance(fit$pooled_model, nhanes_mids, model_fits=fit$selected_model$fit, "bmi", metrics = "cor")), "cor")
  expect_type(perf3, "list")
  expect_type(perf3[[1]], "numeric")
})
