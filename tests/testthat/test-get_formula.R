context("model formula")

test_that("variable names for binary factors are correct",{
  expect_equal(colnames(attr(terms(as.formula(get_formula("hyp", "bmi", nhanes_mids))), "factors")), "hyp")
})