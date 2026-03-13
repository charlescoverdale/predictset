test_that("conformal_jackknife returns correct structure", {
  data <- make_regression_data(50, 2)
  x_new <- matrix(rnorm(10 * 2), ncol = 2)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "jackknife_plus")
  expect_length(result$pred, 10)
  expect_true(all(result$lower <= result$upper))
  expect_length(result$loo_models, 50)
  expect_length(result$loo_residuals, 50)
  expect_true(is.list(result$loo_models))
  expect_true(is.numeric(result$loo_residuals))
  expect_true(all(result$loo_residuals >= 0))
})

test_that("conformal_jackknife basic mode works", {
  data <- make_regression_data(30, 2)
  x_new <- matrix(rnorm(10 * 2), ncol = 2)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, plus = FALSE, seed = 42)

  expect_equal(result$method, "jackknife")
})

test_that("conformal_jackknife works without x_new", {
  data <- make_regression_data(30, 2)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  seed = 42)

  expect_length(result$pred, 30)
})

test_that("jackknife+ produces different intervals from basic jackknife", {
  data <- make_regression_data(50, 2)
  x_new <- matrix(rnorm(10 * 2), ncol = 2)

  r_plus <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, plus = TRUE, seed = 42)
  r_basic <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                   x_new = x_new, plus = FALSE, seed = 42)

  # Point predictions should be the same (both use full model)
  expect_equal(r_plus$pred, r_basic$pred)
  # But intervals should differ (different construction)
  expect_false(all(r_plus$lower == r_basic$lower))
})

test_that("conformal_jackknife achieves approximate coverage", {
  data <- make_regression_data(200, 3)
  n_test <- 100
  x_new <- matrix(rnorm(n_test * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(n_test, sd = 0.5)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, alpha = 0.10, seed = 42)

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.75)
})
