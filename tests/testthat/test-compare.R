test_that("conformal_compare returns correct structure", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)

  comp <- conformal_compare(data$x, data$y, model = test_reg_model,
                              x_new = x_new, y_new = y_new,
                              methods = c("split", "cv"), seed = 42)

  expect_s3_class(comp, "predictset_compare")
  expect_equal(nrow(comp), 2)
  expect_true(all(c("method", "coverage", "mean_width", "median_width",
                     "time_seconds") %in% names(comp)))
})

test_that("conformal_compare has valid values", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)

  comp <- conformal_compare(data$x, data$y, model = test_reg_model,
                              x_new = x_new, y_new = y_new, seed = 42)

  expect_true(all(comp$coverage >= 0 & comp$coverage <= 1))
  expect_true(all(comp$mean_width > 0))
  expect_true(all(comp$time_seconds >= 0))
})

test_that("conformal_compare validates unknown methods", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(10 * 3), ncol = 3)
  y_new <- rnorm(10)

  expect_error(
    conformal_compare(data$x, data$y, model = test_reg_model,
                       x_new = x_new, y_new = y_new,
                       methods = c("split", "bogus")),
    "Unknown method"
  )
})

test_that("conformal_compare print method works", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)

  comp <- conformal_compare(data$x, data$y, model = test_reg_model,
                              x_new = x_new, y_new = y_new, seed = 42)

  # cli outputs to stderr via messages, so capture with expect_message
  expect_invisible(print(comp))
})
