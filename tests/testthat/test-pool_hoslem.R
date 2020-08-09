context("pooling Hosmer-Lemeshow statistic")

test_that("output has expected format", {
  expect_warning(pooled <- pool_hoslem(binom_fit$pooled_model, complete_data, 
                                       outcome="y", prediction=unlist(pred), iter=10), 
                 "(did not converge)|(numerically 0 or 1)|(too few observations)|(may be incorrect)")
  expect_type(pooled, "double")
  expect_length(pooled, 2)
})

test_that("mids object is accepted as input", {
  expect_warning(pooled <- pool_hoslem(binom_fit$pooled_model, binom_mids, 
                                       outcome="y", prediction=unlist(pred), iter=10), 
                 "(did not converge)|(numerically 0 or 1)|(too few observations)|(may be incorrect)")
  expect_type(pooled, "double")
  expect_length(pooled, 2)
})

test_that("predictions are accepted as part of data", {
  expect_warning(pooled <- pool_hoslem(binom_fit$pooled_model, cbind(complete_data, prediction=unlist(pred)),
                                       outcome="y", iter=10), 
                 "(did not converge)|(numerically 0 or 1)|(too few observations)|(may be incorrect)")
  expect_type(pooled, "double")
  expect_length(pooled, 2)
})