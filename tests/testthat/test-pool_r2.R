context("pooling Nagelkerke's R2")

test_that("output has expected format", {
  pooled <- expect_warning(pool_r2(binom_fit$pooled_model, binom_fit$selected_model$fit, complete_data, nrow(binomData)), "(did not converge)|(numerically 0 or 1)")
  expect_type(pooled, "double")
  expect_length(pooled, 2)
})
