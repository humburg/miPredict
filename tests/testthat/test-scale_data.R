context("scaling data")

test_that("numeric variables are scaled", {
  expect_equivalent(scale_data(nhanes2)$bmi, scale(nhanes2$bmi))
})

test_that("factors are unchanged", {
  expect_equal(scale_data(nhanes2)$hyp, nhanes2$hyp)
  expect_equal(scale_data(nhanes_clean)$hyp, nhanes_clean$hyp)
})

test_that("IDs are unchanged", {
  expect_equal(scale_data(nhanes_long)$.id, nhanes_long$.id)
  expect_equal(scale_data(nhanes_long)$.imp, nhanes_long$.imp)
})