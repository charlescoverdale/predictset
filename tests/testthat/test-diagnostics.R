test_that("coverage.predictset_reg works correctly", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  cov <- coverage(result, y_new)
  expect_true(is.numeric(cov))
  expect_true(cov >= 0 && cov <= 1)
})

test_that("coverage validates length", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_error(coverage(result, rnorm(5)))
})

test_that("interval_width works", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  widths <- interval_width(result)
  expect_length(widths, 20)
  expect_true(all(widths >= 0))
})

test_that("interval_width rejects classification objects", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_error(interval_width(result))
})

test_that("set_size works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  sizes <- set_size(result)
  expect_length(sizes, 30)
  expect_true(all(sizes >= 1))
})

test_that("set_size rejects regression objects", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_error(set_size(result))
})

test_that("coverage.predictset_class works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  y_new <- factor(sample(c("A", "B"), 30, replace = TRUE), levels = c("A", "B"))

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  cov <- coverage(result, y_new)
  expect_true(is.numeric(cov))
  expect_true(cov >= 0 && cov <= 1)
})
