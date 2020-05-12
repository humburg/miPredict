context("bootstrapping")

test_that("progress is reported", {
  expect_message(suppressWarnings(boot_model(binom_mids, "y", iter = 1)), "Iteration", all=TRUE)
})
