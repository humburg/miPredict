context("Pooling Brier Score")

test_that("output has expected format", {
  d <- complete(binom_mids, action="long")
  pred <- by(d, d$.imp, function(x) predict(binom_fit$pooled_model, newdata = x, type = "response"))
  brier <- lapply(pred, BrierDecomp, binomData$y)
  pooled <- pool_brier(brier, nrow(binomData))
  expect_length(pooled, 6)
  expect_type(pooled, "double")
  expect_equal(dim(pooled), c(2, 3))
})