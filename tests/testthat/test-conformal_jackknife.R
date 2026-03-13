test_that("conformal_jackknife returns correct structure", {
  data <- make_regression_data(50, 2)
  x_new <- matrix(rnorm(10 * 2), ncol = 2)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "jackknife_plus")
  expect_length(result$pred, 10)
  expect_true(all(result$lower <= result$upper))
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
