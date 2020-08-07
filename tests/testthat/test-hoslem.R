context("Hosmer-Lemeshow statistic")

test_that("output has expected format", {
  expect_warning(hl <- hoslem(complete_data, "y", prediction=unlist(pred)), "(incorrect)|(few observations)")
  expect_length(hl, 5)
  expect_named(hl[[1]], c("statistic", "parameter", "p.value", "method", "data.name", "observed", "expected", "stddiffs"))
  expect_s3_class(hl[[1]], "htest")
})

test_that("predictions can be included with data", {
  expect_warning(hl <- hoslem(cbind(complete_data, prediction=unlist(pred)), "y"), "(incorrect)|(few observations)")
  expect_length(hl, 5)
  expect_named(hl[[1]], c("statistic", "parameter", "p.value", "method", "data.name", "observed", "expected", "stddiffs"))
  expect_s3_class(hl[[1]], "htest")
})

test_that("missing predictions are detected", {
  expect_error(hoslem(complete_data, "y"), "prediction")
})