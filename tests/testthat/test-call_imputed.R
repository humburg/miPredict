context("Summarizing imputed outcomes")

test_that("output has expected structure", {
  expect_length(calls <- call_imputed(nhanes_mids, "hyp"), nrow(nhanes_large))
  expect_equal(sum(is.na(calls)), 0)
  expect_equal(sum(!calls %in% c(0,1)), 0)
})

test_that("results are unchanged", {
  expect_equivalent(unclass(table(call_imputed(nhanes_mids, "hyp"))), c(68, 32))
})