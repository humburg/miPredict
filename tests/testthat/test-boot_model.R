context("bootstrapping")

suppressWarnings(boot_result <- boot_model(binom_mids, "y", iter=3))
  
test_that("progress is reported", {
  expect_message(suppressWarnings(boot_model(binom_mids, "y", iter = 1)), "Iteration", all=TRUE)
})

test_that("output has expected structure", {
  expect_type(boot_result, "list")
  expect_length(boot_result, 3)
  expect_true(all(sapply(boot_result, function(x) "selected_model" %in% names(x))))
  expect_true(all(sapply(boot_result, function(x) "pooled_model" %in% names(x))))
  expect_true(all(sapply(boot_result, function(x) "data" %in% names(x))))
})

test_that("multiple imputation can be skipped",{
  expect_message(suppressWarnings(boot_nomi <- boot_model(binom_mids, "y", iter = 3, do_impute = FALSE)), "Iteration", all=TRUE)
  expect_length(boot_nomi, 3)
  expect_true(all(sapply(boot_nomi, function(x) "selected_model" %in% names(x))))
  expect_true(all(sapply(boot_nomi, function(x) "pooled_model" %in% names(x))))
  expect_true(all(sapply(boot_nomi, function(x) "data" %in% names(x))))
})