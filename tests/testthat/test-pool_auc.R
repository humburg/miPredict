context("pooled AUC estimates")

suppressWarnings(perf <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "roc"))
suppressWarnings(auc <- pool_auc(perf[["roc"]]))

test_that("output has expected structure", {
  expect_equal(class(auc), "list")
  expect_equal(length(auc), 2)
  expect_equal(names(auc), c("estimate", "ci"))
  expect_equal(class(auc[[1]]), "numeric")
  expect_equal(class(auc[[2]]), "numeric")
})

test_that("output matches expectation", {
  expect_equal(auc, list(estimate=1, ci=c(1,1)))
})
