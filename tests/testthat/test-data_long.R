context("data in long format")

test_that("long data is unchanged", {
  expect_equal(data_long(nhanes_long, clean=FALSE), nhanes_long)
})

test_that("multi-level factors are expanded", {
  expect_equal(ncol(nhanes_long)+1, ncol(data_long(nhanes_long, clean=TRUE)))
})

test_that("imputed data is converted to long format", {
  expect_equal(data_long(nhanes_mids), nhanes_clean)
  expect_true(".imp" %in% names(data_long(nhanes_mids)))
})

test_that("long data has '.imp' column", {
  expect_error(data_long(nhanes2), "Expected data frame of complete data exported from `mice`")
})