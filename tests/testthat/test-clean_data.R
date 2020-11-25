context("data cleaning")

test_that("binary factors are converted to 0,1", {
  expect_equal(sort(unique(clean_data(nhanes_long)$hyp)), c(0,1))
})

test_that("multilevel factors are dummy coded", {
  expect_named(clean_data(nhanes_long), c(".imp", ".id", "bmi", "chl", "age40.59", "age60.99", "hyp"))
})

test_that("numeric values are unchanged",{
  expect_equal(clean_data(nhanes_long)$bmi, nhanes_long$bmi)
})
