test_that("conformal_cv returns correct structure", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_cv(data$x, data$y, model = test_reg_model,
                          x_new = x_new, n_folds = 5, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "cv_plus")
  expect_length(result$pred, 20)
  expect_true(all(result$lower <= result$upper))
})

test_that("conformal_cv works without x_new", {
  data <- make_regression_data(100, 3)

  result <- conformal_cv(data$x, data$y, model = test_reg_model,
                          n_folds = 5, seed = 42)

  expect_length(result$pred, 100)
})

test_that("conformal_cv achieves approximate coverage", {
  data <- make_regression_data(500, 3)
  n_test <- 200
  x_new <- matrix(rnorm(n_test * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(n_test, sd = 0.5)

  result <- conformal_cv(data$x, data$y, model = test_reg_model,
                          x_new = x_new, n_folds = 5, alpha = 0.10, seed = 42)

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.75)
})

test_that("conformal_cv validates n_folds", {
  data <- make_regression_data(100, 3)
  expect_error(conformal_cv(data$x, data$y, test_reg_model, n_folds = 1))
  expect_error(conformal_cv(data$x, data$y, test_reg_model, n_folds = 101))
})
