context("bootstrapping")

suppressWarnings(boot_result <- boot_model(binom_mids, "y", iter=3, metrics="brier"))
  
test_that("progress is reported", {
  expect_message(suppressWarnings(boot_model(binom_mids, "y", iter = 1)), "Iteration", all=TRUE)
})

test_that("output has expected structure", {
  expect_type(boot_result, "list")
  expect_length(boot_result, 3)
  expect_true(all(sapply(boot_result, function(x) "selected_model" %in% names(x))))
  expect_true(all(sapply(boot_result, function(x) "pooled_model" %in% names(x))))
  expect_true(all(sapply(boot_result, function(x) "brier" %in% names(x))))
  expect_type(boot_result[[1]]$brier, "double")
  expect_type(boot_result[[1]]$r2, "NULL")
})

test_that("multiple imputation can be skipped",{
  expect_message(suppressWarnings(boot_nomi <- boot_model(binom_mids, "y", iter = 3, do_impute = FALSE)), "Iteration", all=TRUE)
  expect_length(boot_nomi, 3)
  expect_true(all(sapply(boot_nomi, function(x) "selected_model" %in% names(x))))
  expect_true(all(sapply(boot_nomi, function(x) "pooled_model" %in% names(x))))
})

test_that("data.frame is rejected as input", {
  expect_error(suppressWarnings(boot_model(mice::complete(binom_mids, action="long"), "y", iter = 1), "mids"))
})

test_that("only expected warnings occur", {
  expect_warning(boot_model(binom_mids, "y", iter=1, metrics="brier", scale=TRUE), "(not converge)|(0 or 1)|(scale = TRUE)", all=TRUE)
})