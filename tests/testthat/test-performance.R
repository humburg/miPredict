context("model performance")

test_that("output has expected structure", {
  expect_named(suppressWarnings(perf1 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "roc")), "roc")
  expect_equal(class(perf1$roc[[1]]), "roc")
  expect_equal(length(perf1$roc), binom_mids$m)
  
  expect_named(suppressWarnings(perf2 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc"))), c("roc", "auc"))
  expect_equal(class(perf2$auc), "numeric")
  
  expect_named(suppressWarnings(perf2a <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = "auc")), "auc")
  expect_equal(names(perf2a), "auc")
  expect_equal(class(perf2a$auc), "numeric")
  expect_equal(length(perf2a$auc), 3)
  
  expect_named(suppressWarnings(perf3 <- performance(binom_fit$pooled_model, binom_mids, "y", 
                                                     metrics = c("roc", "auc", "brier"))), c("roc", "auc", "brier"))
  expect_equal(class(perf3$brier[[1]]), "numeric")
  expect_equal(length(perf3$brier), 8)
  
  expect_named(suppressWarnings(perf4 <- performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2"), 
                                                     binom_fit$selected_model$fit)), c("roc", "auc", "brier", "r2"))
  expect_equal(class(perf4$r2[[1]]), "numeric")
  expect_equal(length(perf4$r2), binom_mids$m)
  
  expect_named(suppressWarnings(perf5 <- performance(binom_fit$pooled_model, binom_mids, "y", 
                                                     metrics = c("roc", "auc", "brier", "hoslem"))), 
               c("roc", "auc", "brier", "hoslem"))
  expect_equal(class(perf5$hoslem[[1]]), "htest")
  expect_equal(length(perf5$hoslem), binom_mids$m)
})

test_that("missing arguments are handled", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", metrics = c("roc", "auc", "brier", "r2")), "model_fits")
})

test_that("performance metrics fail gracefully", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", "hoslem", g=0), "Test failed")
})

test_that("arguments are passed on", {
  expect_warning(performance(binom_fit$pooled_model, binom_mids, "y", "hoslem", g=5), "compute 5 rows")
})