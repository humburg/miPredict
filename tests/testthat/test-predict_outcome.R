context("predictions")

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
  expect_known_output(print(round(predictions, 5)), "predict_outcome.out")
})