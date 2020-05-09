context("data cleaning")

library(mice)
data(nhanes)
nhanes_mids <- mice(nhanes2, m=5, printFlag=FALSE) 
nhanes_long <- complete(nhanes_mids, action="long")

test_that("binary factors are converted to 0,1", {
  expect_equal(sort(unique(clean_data(nhanes_long)$hyp)), c(0,1))
})

test_that("numeric values are unchanged",{
  expect_equal(clean_data(nhanes_long)$bmi, nhanes_long$bmi)
})
