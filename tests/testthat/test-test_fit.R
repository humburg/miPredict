context("bootstrap performance estimates")

metrics <- c("auc", "specificity", "sensitivity", "accuracy", "precision")
test_that("output contains requested measures", {
  expect_length(perf <- test_fit(binom_fit$pooled_model, binomData_complete, "y"), length(metrics))
  expect_named(perf, metrics)
  expect_equivalent(sapply(perf, length), rep(3, 5))
  expect_length(perf2 <- test_fit(binom_fit$pooled_model, binomData_complete, "y", metrics=metrics[1:2]), 2)
  expect_named(perf2, metrics[1:2])
  expect_length(perf_boot <- test_fit(binom_fit$pooled_model, binomData_complete, "y", metrics=metrics[1:2], bootstrap=TRUE, iter=10), 2)
  expect_named(perf_boot, metrics[1:2])
})

test_that("imputed data is accepted as input", {
  expect_length(perf <- test_fit(binom_fit$pooled_model, binom_mids, "y"), length(metrics))
  expect_named(perf, metrics)
  expect_equivalent(sapply(perf, length), rep(3, 5))
  expect_length(perf2 <- test_fit(binom_fit$pooled_model, binom_mids, "y", metrics=metrics[1:2]), 2)
  expect_named(perf2, metrics[1:2])
  expect_length(perf_boot <- test_fit(binom_fit$pooled_model, binom_mids, "y", metrics=metrics[1:2], bootstrap=TRUE, iter=10), 2)
  expect_named(perf_boot, metrics[1:2])
})