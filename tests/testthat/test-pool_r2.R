context("pooling R2")

test_that("Nagelkerke's R2 can be selected", {
  set.seed(12)
  pooled1 <- expect_warning(pool_r2(binom_fit$pooled_model, binom_fit$selected_model$fit, complete_data, nrow(binomData)), "(did not converge)|(numerically 0 or 1)")
  set.seed(12)
  pooled2 <- expect_warning(pool_r2(binom_fit$pooled_model, binom_fit$selected_model$fit, complete_data, nrow(binomData), method="n"), "(did not converge)|(numerically 0 or 1)")
  expect_equal(pooled1, pooled2)
})

test_that("Nagelkerke's R2 output has expected format", {
  pooled <- expect_warning(pool_r2(binom_fit$pooled_model, binom_fit$selected_model$fit, complete_data, nrow(binomData)), "(did not converge)|(numerically 0 or 1)")
  expect_type(pooled, "double")
  expect_length(pooled, 2)
})

test_that("R2 has expected format", {
  fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE)
  pooled <- pool_r2(fit$pooled_model, fit$selected_model$fit, nhanes_long, method="r2")
  expect_type(pooled, "double")
  expect_length(pooled, 4)
  expect_true(pooled[1,1] > pooled[1,2])
  expect_true(pooled[1,2] < pooled[1,3])
})

test_that("Adjusted R2 has expected format", {
  fit <- fit_model(nhanes_mids, outcome="bmi", family="gaussian", scale=TRUE)
  pooled <- pool_r2(fit$pooled_model, fit$selected_model$fit, nhanes_long, method="adj.r2")
  expect_type(pooled, "double")
  expect_length(pooled, 4)
})
