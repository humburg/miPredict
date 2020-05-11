context("model performance")

perf1 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "roc")
perf2 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc"))
perf3 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier"))
perf4 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2"), binom_fit$selected_model$fit)

test_that("output has expected format", {
  expect_equal(length(perf1), 1)
  expect_equal(names(perf1), "roc")
  expect_equal(length(perf2), 2)
  expect_equal(names(perf2), c("roc", "auc"))
  expect_equal(length(perf3), 3)
  expect_equal(names(perf3), c("roc", "auc", "brier"))
  expect_equal(length(perf4), 4)
  expect_equal(names(perf4), c("roc", "auc", "brier", "r2"))
})

test_that("missing arguments are handled", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2")), "model_fits")
})