context("Pooling Brier Score")

d <- complete(binom_mids, action="long")
pred <- by(d, d$.imp, function(x) predict(binom_fit$pooled_model, newdata = x, type = "response"))

test_that("output has expected format", {
  brier <- lapply(pred, BrierDecomp, binomData$y)
  pooled <- pool_brier(brier, nrow(binomData))
  expect_length(pooled, 8)
  expect_type(pooled, "double")
  expect_equal(dim(pooled), c(2, 4))
})

test_that("computing total is optional", {
  brier <- lapply(pred, BrierDecomp, binomData$y)
  pooled <- pool_brier(brier, nrow(binomData), addTotal=FALSE)
  expect_length(pooled, 6)
  expect_type(pooled, "double")
  expect_equal(dim(pooled), c(2, 3))
})