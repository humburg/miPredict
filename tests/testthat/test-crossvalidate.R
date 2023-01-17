context("cross-validation")

common_warn <- "(not converge)|(0 or 1)|(scale = TRUE)"

test_that("output has expected structure", {
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2))
  expect_s3_class(cv, "cv")
  expect_length(cv, 2)
  expect_named(cv, c("pooled", "imputed"))
  expect_equal(nrow(cv$pooled), nrow(nhanes_large))
  expect_length(cv$pooled, 3)
  expect_named(cv$pooled, c("prediction", "se", "hyp"))
  expect_gte(min(cv$pooled$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$pooled$prediction, na.rm=TRUE), 1)
  expect_equal(sum(sapply(cv$imputed, nrow)), nrow(nhanes_large)*nhanes_mids$m)
  expect_length(cv$imputed, nhanes_mids$m)
  expect_named(cv$imputed[[1]], c("hyp", "prediction", "se"))
  expect_gte(min(cv$imputed[[1]]$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$imputed[[1]]$prediction, na.rm=TRUE), 1)
})

test_that("leave-one-out works", {
  skip_on_ci()
  skip_on_cran()
  expect_warning(cv <- crossvalidate(nhanes_mids, "hyp", k=100), common_warn)
  expect_length(cv$imputed, nhanes_mids$m)
  expect_equal(nrow(cv$imputed[[1]]), nrow(nhanes_large))
})

test_that("low number of outcomes is flagged", {
  expect_error(cv <- crossvalidate(small_mids, "y", k=5), "Too few observations")
  expect_warning(cv <- crossvalidate(small_mids, "y", k=5, force=TRUE), paste("(Too few observations)", common_warn, sep="|"))
})

test_that("lack of missing values in training set raises a warning", {
  expect_warning(cv <- crossvalidate(small_mids2, "y", k=9, force=TRUE), paste("(no missing)|(Too few observations)", common_warn, sep="|"))
})

test_that("cross-validation works with fixed model", {
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2, predictors="bmi"))
  expect_s3_class(cv, "cv")
  expect_length(cv, 2)
  expect_named(cv, c("pooled", "imputed"))
  expect_equal(nrow(cv$pooled), nrow(nhanes_large))
  expect_length(cv$pooled, 3)
  expect_named(cv$pooled, c("prediction", "se", "hyp"))
  expect_gte(min(cv$pooled$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$pooled$prediction, na.rm=TRUE), 1)
  expect_equal(sum(sapply(cv$imputed, nrow)), nrow(nhanes_large)*nhanes_mids$m)
  expect_length(cv$imputed, nhanes_mids$m)
  expect_named(cv$imputed[[1]], c("hyp", "prediction", "se"))
  expect_gte(min(cv$imputed[[1]]$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$imputed[[1]]$prediction, na.rm=TRUE), 1)
})

test_that("cross-validation works with fixed categorical predictors", {
  set.seed(117)
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2, predictors="age"))
  expect_s3_class(cv, "cv")
  expect_length(cv, 2)
  expect_named(cv, c("pooled", "imputed"))
  expect_equal(nrow(cv$pooled), nrow(nhanes_large))
  expect_length(cv$pooled, 3)
  expect_named(cv$pooled, c("prediction", "se", "hyp"))
  expect_gte(min(cv$pooled$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$pooled$prediction, na.rm=TRUE), 1)
  expect_equal(sum(sapply(cv$imputed, nrow)), nrow(nhanes_large)*nhanes_mids$m)
  expect_length(cv$imputed, nhanes_mids$m)
  expect_named(cv$imputed[[1]], c("hyp", "prediction", "se"))
  expect_gte(min(cv$imputed[[1]]$prediction, na.rm=TRUE), 0)
  expect_lte(max(cv$imputed[[1]]$prediction, na.rm=TRUE), 1)
})

