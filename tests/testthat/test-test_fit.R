context("bootstrap performance estimates")

test_that("output contains requested measures", {
  metrics <- c("auc", "thresholds", "sp", "se", "coords")
  expect_length(perf <- test_fit(binom_fit$pooled_model, binomData_complete), length(metrics))
  expect_named(perf, metrics)
  expect_length(perf2 <- test_fit(binom_fit$pooled_model, binomData_complete, which=metrics[1:2]), 2)
  expect_named(perf2, metrics[1:2])
  expect_length(perf_boot <- test_fit(binom_fit$pooled_model, binomData_complete, which=metrics[1:2], bootstrap=TRUE, iter=10), 2)
  testthat::expect_named(perf_boot, metrics[1:2])
})
