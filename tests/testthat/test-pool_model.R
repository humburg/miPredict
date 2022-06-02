context("model pooling")

test_that("pooled model family is set correctly", {
  fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE)
  expect_equal(fit$pooled_model$family$family, "gaussian")
  expect_equal(binom_fit$pooled_model$family$family, "binomial")
})