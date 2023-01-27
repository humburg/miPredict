context("model formula")

test_that("variable names for binary factors are correct",{
  expect_silent(form <- get_formula("hyp", "bmi", nhanes_mids))
  expect_equal(colnames(attr(terms(as.formula(form)), "factors")), "hyp")
})

test_that("variable names for multi-level factors are correct",{
  expect_equal(colnames(attr(terms(as.formula(get_formula("age", "bmi", nhanes_mids))), "factors")), c("age40.59", "age60.99"))
})
