test_that("conformal_split returns correct structure", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "split")
  expect_equal(result$alpha, 0.10)
  expect_length(result$pred, 50)
  expect_length(result$lower, 50)
  expect_length(result$upper, 50)
  expect_true(all(result$lower <= result$upper))
})

test_that("conformal_split achieves approximate coverage", {
  data <- make_regression_data(2000, 5)
  n_test <- 500
  x_new <- matrix(rnorm(n_test * 5), ncol = 5)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(n_test, sd = 0.5)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, alpha = 0.10, seed = 42)

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.80)
})

test_that("conformal_split works with formula", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = y ~ .,
                             x_new = x_new, seed = 42)
  expect_s3_class(result, "predictset_reg")
})

test_that("conformal_split works with fitted lm", {
  data <- make_regression_data(100, 3)
  fit <- lm(y ~ ., data = data.frame(y = data$y, data$x))
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = fit,
                             x_new = x_new, seed = 42)
  expect_s3_class(result, "predictset_reg")
})

test_that("conformal_split is reproducible with seed", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  r1 <- conformal_split(data$x, data$y, model = test_reg_model,
                          x_new = x_new, seed = 42)
  r2 <- conformal_split(data$x, data$y, model = test_reg_model,
                          x_new = x_new, seed = 42)

  expect_equal(r1$pred, r2$pred)
  expect_equal(r1$lower, r2$lower)
})

test_that("conformal_split validates inputs", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  expect_error(conformal_split("bad", data$y, test_reg_model, x_new))
  expect_error(conformal_split(data$x, "bad", test_reg_model, x_new))
  expect_error(conformal_split(data$x, data$y, test_reg_model, x_new, alpha = 0))
  expect_error(conformal_split(data$x, data$y, test_reg_model, x_new, alpha = 1))
  expect_error(conformal_split(data$x, data$y[1:5], test_reg_model, x_new))
})

test_that("conformal_quantile returns Inf for extreme alpha", {
  scores <- c(1, 2, 3)
  # alpha = 0.01 -> k = ceiling(4 * 0.99) = 4 > 3
  q <- predictset:::conformal_quantile(scores, 0.01)
  expect_equal(q, Inf)
})

test_that("conformal_split with single test point", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)
  expect_length(result$pred, 1)
})
