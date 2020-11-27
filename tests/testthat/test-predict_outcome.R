context("predictions")
set.seed(1764)
predictions <- predict_outcome(binom_fit$pooled_model, binom_mids)

test_that("output has expected structure", {
  expect_equal(class(predictions)[1], "matrix")
  expect_equal(dim(predictions), c(nrow(binomData), binom_mids$m))
})

test_that("data frames are handled correctly", {
  expect_equal(predict_outcome(binom_fit$pooled_model, mice::complete(binom_mids, action="long")), predictions)
  expect_error(predict_outcome(binom_fit$pooled_model, binomData), "mice::complete\\(data, action='long'\\)")
})

test_that("results have not changed",{
  skip_on_ci()
  expect_known_output(print(round(predictions, 5)), "predict_outcome.out")
})

test_that("multi-level factors are handled correctly", {
  suppressWarnings(model <- fit_model(nhanes_mids, outcome="hyp", scale=TRUE))
  expect_silent(pred <- predict_outcome(model$pooled_model, nhanes_long))
})