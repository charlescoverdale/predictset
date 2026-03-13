test_that("conformal_weighted returns correct structure", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)
  weights <- rep(1, 200)

  result <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                x_new = x_new, weights = weights, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "weighted")
  expect_length(result$pred, 30)
  expect_true(all(result$lower <= result$upper))
})

test_that("uniform weights match standard split conformal", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)
  weights <- rep(1, 200)

  result_w <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, weights = weights, seed = 42)
  result_s <- conformal_split(data$x, data$y, model = test_reg_model,
                               x_new = x_new, seed = 42)

  # Same predictions (same split, same model)
  expect_equal(result_w$pred, result_s$pred)
  expect_equal(result_w$scores, result_s$scores)
})

test_that("NULL weights defaults to uniform", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)

  result_null <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                     x_new = x_new, weights = NULL, seed = 42)
  result_ones <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                     x_new = x_new,
                                     weights = rep(1, 200), seed = 42)

  expect_equal(result_null$pred, result_ones$pred)
  expect_equal(result_null$quantile, result_ones$quantile)
})

test_that("conformal_weighted validates weights", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(10 * 3), ncol = 3)

  expect_error(
    conformal_weighted(data$x, data$y, model = test_reg_model,
                        x_new = x_new, weights = rep(1, 50)),
    "weights"
  )

  expect_error(
    conformal_weighted(data$x, data$y, model = test_reg_model,
                        x_new = x_new, weights = rep(-1, 100)),
    "non-negative"
  )
})

test_that("conformal_weighted achieves approximate coverage", {
  data <- make_regression_data(1000, 5)
  n_test <- 300
  x_new <- matrix(rnorm(n_test * 5), ncol = 5)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(n_test, sd = 0.5)

  result <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                x_new = x_new, seed = 42)
  cov <- coverage(result, y_new)
  expect_gt(cov, 0.80)
})
