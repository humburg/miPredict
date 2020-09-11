context("bootstrap performance estimates")

metrics <- c("auc", "specificity", "sensitivity", "accuracy", "precision")
test_that("output contains requested measures", {
  expect_length(perf <- class_perf(predict(binom_fit$pooled_model, newdata=binomData_complete), binomData_complete$y), length(metrics))
  expect_named(perf, metrics)
  expect_equivalent(sapply(perf, length), rep(3, 5))
  expect_length(perf2 <- class_perf(predict(binom_fit$pooled_model, newdata=binomData_complete), binomData_complete$y, metrics=metrics[1:2]), 2)
  expect_named(perf2, metrics[1:2])
  expect_length(perf_boot <- class_perf(predict(binom_fit$pooled_model, newdata=binomData_complete), binomData_complete$y, metrics=metrics[1:2], bootstrap=TRUE, iter=10), 2)
  expect_named(perf_boot, metrics[1:2])
})
