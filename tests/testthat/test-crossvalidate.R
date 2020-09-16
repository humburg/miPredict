context("cross-validation")

common_warn <- "(not converge)|(0 or 1)|(scale = TRUE)"

test_that("output has expected structure", {
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2))
  expect_s3_class(cv, "cv")
  expect_equal(nrow(cv), nrow(nhanes_large))
  expect_length(cv, 2)
  expect_named(cv, c("prediction", "se"))
  expect_gte(min(cv$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$prediction, na.rm=TRUE), 1)
})

test_that("leave-one-out works", {
  skip_on_ci()
  skip_on_cran()
  expect_warning(cv <- crossvalidate(nhanes_mids, "hyp", k=100), common_warn)
  expect_equal(nrow(cv), nrow(nhanes_large))
})

test_that("low number of outcomes is flagged", {
  expect_error(cv <- crossvalidate(small_mids, "y", k=5), "Too few observations")
  expect_warning(cv <- crossvalidate(small_mids, "y", k=5, force=TRUE), paste("(Too few observations)", common_warn, sep="|"))
})

test_that("lack of missing values in training set raises a warning", {
  expect_warning(cv <- crossvalidate(small_mids2, "y", k=9, force=TRUE), paste("(no missing)|(Too few observations)", common_warn, sep="|"))
})
