context("cross-validation")

common_warn <- "(not converge)|(0 or 1)|(scale = TRUE)"

test_that("output has expected structure", {
  cv <- suppressWarnings(crossvalidate(binom_mids, "y", k=5))
  expect_length(cv, nrow(binomData))
})

test_that("leave-one-out works", {
  skip_on_ci()
  skip_on_cran()
  expect_warning(cv <- crossvalidate(binom_mids, "y", k=100), common_warn)
  expect_length(cv, nrow(binomData))
})

test_that("excessive number of folds causes error", {
  expect_error(cv <- crossvalidate(small_mids, "y", k=5), "Too few observations")
})

test_that("lack of missing values in training set raises a warning", {
  expect_warning(cv <- crossvalidate(small_mids2, "y", k=9), paste("(no missing)", common_warn, sep="|"))
})
