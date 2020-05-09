context("data in long format")

library(mice)
data(nhanes)
nhanes_mids <- mice(nhanes2, m=5, printFlag=FALSE) 
nhanes_long <- complete(nhanes_mids, action="long")
nhanes_clean <- nhanes_long %>% clean_data()

test_that("long data is unchanged", {
  expect_equal(data_long(nhanes_long), nhanes_long)
})

test_that("imputed data is converted to long format", {
  expect_equal(data_long(nhanes_mids), nhanes_clean)
  expect_true(".imp" %in% names(data_long(nhanes_mids)))
})

test_that("long data has '.imp' column", {
  expect_error(data_long(nhanes2), "Expected data frame of complete data exported from `mice`")
})