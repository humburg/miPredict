context("Model fitting")

test_that("output has expected structure", {
  expect_silent(fit <- fit_model(nhanes_mids, outcome="hyp", scale=TRUE))
  expect_named(fit, c("selected_model", "pooled_model"))
  expect_named(fit$selected_model, c("formula", "fit"))
  expect_s3_class(fit$selected_model$formula, "formula")
  expect_length(fit$selected_model$fit, nhanes_mids$m)
  expect_s3_class(fit$pooled_model, "binomial")
  expect_equal(attr(fit$pooled_model, "scale"), TRUE)
})

test_that("unscaled data triggers warning", {
  expect_warning(fit <- fit_model(nhanes_mids, outcome="hyp"), "scale = TRUE")
})

test_that("fixed model can be used", {
  expect_silent(fit <- fit_model(nhanes_mids, outcome="hyp", predictors="bmi"))
  expect_named(fit, c("selected_model", "pooled_model"))
  expect_named(fit$selected_model, c("formula", "fit"))
  expect_s3_class(fit$selected_model$formula, "formula")
  expect_length(fit$selected_model$fit, nhanes_mids$m)
  expect_s3_class(fit$pooled_model, "binomial")
})

test_that("fixed model with factor predictors works", {
  expect_silent(fit <- fit_model(nhanes_mids, outcome="hyp", predictors="age"))
  expect_named(fit, c("selected_model", "pooled_model"))
  expect_named(fit$selected_model, c("formula", "fit"))
  expect_s3_class(fit$selected_model$formula, "formula")
  expect_length(fit$selected_model$fit, nhanes_mids$m)
  expect_s3_class(fit$pooled_model, "binomial")
})

test_that("predictors can be forced into the model", {
  expect_warning(fit_free <- fit_model(binom_mids, outcome="y", scale=TRUE), "0 or 1|converge")
  expect_warning(fit <- fit_model(binom_mids, outcome="y", include="V1", scale=TRUE), "0 or 1|converge")
  expect_gt(length(coef(fit$pooled_model)), 2)
  expect_true("V1" %in% names(coef(fit$pooled_model)))
  testthat::expect_true(!all(coef(fit$pooled_model) %in% coef(fit_free$pooled_model)))
})

test_that("Gaussian models are supported", {
  set.seed(64)
  expect_silent(fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE))
  expect_equal(names(fit), c("selected_model", "pooled_model"))
  expect_length(fit$selected_model$fit, 5)
  expect_length(coef(fit$selected_model$fit[[1]]), 4)
  expect_length(coef(fit$pooled_model), 4)
  expect_equal(class(fit$pooled_model), c("gaussian", "glm", "lm"))
})