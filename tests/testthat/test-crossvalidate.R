context("cross-validation")

test_that("output has expected structure", {
  cv <- suppressWarnings(crossvalidate(binom_mids, "y", k=5))
  expect_length(cv, nrow(binomData))
})

test_that("leave-one-out works", {
  skip_on_ci()
  skip_on_cran()
  expect_warning(cv <- crossvalidate(binom_mids, "y", k=100), "(not converge)|(0 or 1)|(scale = TRUE)")
  expect_length(cv, nrow(binomData))
})

