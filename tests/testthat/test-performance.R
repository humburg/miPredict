context("model performance")

perf1 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "roc")
perf2 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc"))
perf3 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier"))
perf4 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2"), binom_fit$selected_model$fit)

test_that("output has expected structure", {
  expect_equal(length(perf1), 1)
  expect_equal(names(perf1), "roc")
  expect_equal(class(perf1$roc[[1]]), "roc")
  expect_equal(length(perf1$roc), binom_mids$m)
  expect_equal(length(perf2), 2)
  expect_equal(names(perf2), c("roc", "auc"))
  expect_equal(class(perf2$auc[[1]]), "numeric")
  expect_equal(dim(perf2$auc), c(binom_mids$m, 3))
  expect_equal(length(perf3), 3)
  expect_equal(names(perf3), c("roc", "auc", "brier"))
  expect_equal(class(perf3$brier[[1]]), "numeric")
  expect_equal(length(perf3$brier), binom_mids$m)
  expect_equal(length(perf4), 4)
  expect_equal(names(perf4), c("roc", "auc", "brier", "r2"))
  expect_equal(class(perf4$r2[[1]]), "numeric")
  expect_equal(length(perf4$r2), binom_mids$m)
})

test_that("missing arguments are handled", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2")), "model_fits")
})