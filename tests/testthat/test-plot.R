context("Plotting")

test_that("plots are generated", {
  set.seed(93)
  cv <- suppressWarnings(crossvalidate(nhanes_mids, "hyp", k=2))
  expect_warning(fig <- plot(cv), "geom_text")
  expect_s3_class(fig, "gtable")
})
