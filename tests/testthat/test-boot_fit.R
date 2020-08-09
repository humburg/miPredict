context("refit model on bootstrapped data")

test_that("model fitting works", {
  suppressWarnings(fit2 <- boot_fit(binom_fit$pooled_model, subset(complete_data, .imp == 1)))
  expect_s3_class(fit2, "glm")
  expect_equal(family(binom_fit$pooled_model), family(fit2))
  expect_equal(formula(binom_fit$pooled_model), formula(fit2))
})
