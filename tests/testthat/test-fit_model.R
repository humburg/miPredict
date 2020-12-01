context("Model fitting")

test_that("output has expected structure", {
  expect_warning(fit <- fit_model(nhanes_mids, outcome="hyp", scale=TRUE), "0 or 1")
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