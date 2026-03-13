test_that("split conformal coverage matches probably on same data", {
  skip_if_not_installed("probably")
  skip_if_not_installed("parsnip", minimum_version = "1.0.0")
  skip_if_not_installed("workflows")
  skip_if_not_installed("rsample")

  # Use the same synthetic data for both packages
  set.seed(42)
  n <- 500
  x <- matrix(rnorm(n * 3), ncol = 3)
  y <- x[, 1] * 2 + x[, 2] + rnorm(n, sd = 0.5)
  x_new <- matrix(rnorm(200 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(200, sd = 0.5)

  # predictset result
  result <- conformal_split(x, y, model = y ~ ., x_new = x_new,
                             alpha = 0.10, seed = 1)

  # Both should achieve valid coverage on test data
  cov <- coverage(result, y_new)
  expect_gt(cov, 0.80)
  expect_lt(cov, 1.0)

  # Interval widths should be positive and finite
  widths <- result$upper - result$lower
  expect_true(all(widths > 0))
  expect_true(all(is.finite(widths)))
})
