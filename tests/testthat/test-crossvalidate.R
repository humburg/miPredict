context("cross-validation")

common_warn <- "(not converge)|(0 or 1)|(scale = TRUE)"

test_that("output has expected structure", {
  cv <- suppressWarnings(crossvalidate(binom_mids, "y", k=5))
  expect_length(cv, nrow(binomData))
  expect_gte(min(cv, na.rm=TRUE), 0)
  expect_lte(max(cv, na.rm=TRUE), 1)
})

test_that("leave-one-out works", {
  skip_on_ci()
  skip_on_cran()
  expect_warning(cv <- crossvalidate(binom_mids, "y", k=100), common_warn)
  expect_length(cv, nrow(binomData))
})

test_that("low number of outcomes is flagged", {
  expect_error(cv <- crossvalidate(small_mids, "y", k=5), "Too few observations")
  expect_warning(cv <- crossvalidate(small_mids, "y", k=5, force=TRUE), paste("(Too few observations)", common_warn, sep="|"))
})

test_that("lack of missing values in training set raises a warning", {
  expect_warning(cv <- crossvalidate(small_mids2, "y", k=9, force=TRUE), paste("(no missing)|(Too few observations)", common_warn, sep="|"))
})
