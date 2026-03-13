test_that("print.predictset_reg works", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_no_error(capture.output(print(result), type = "message"))
  expect_invisible(print(result))
})

test_that("print.predictset_class works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_no_error(capture.output(print(result), type = "message"))
  expect_invisible(print(result))
})

test_that("summary.predictset_reg works", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_no_error(capture.output(summary(result), type = "message"))
  expect_invisible(summary(result))
})

test_that("summary.predictset_class works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_no_error(capture.output(summary(result), type = "message"))
  expect_invisible(summary(result))
})
