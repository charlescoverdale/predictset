test_that("conformal_lac returns correct structure", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y,
                           model = test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_s3_class(result, "predictset_class")
  expect_equal(result$method, "lac")
  expect_length(result$sets, 30)
})

test_that("conformal_lac achieves approximate coverage", {
  data <- make_classification_data(500, 4, n_classes = 2)
  n_test <- 200
  test_data <- make_classification_data(n_test, 4, n_classes = 2, seed = 999)

  result <- conformal_lac(data$x, data$y,
                           model = test_class_model_binary,
                           x_new = test_data$x, alpha = 0.10, seed = 42)

  cov <- coverage(result, test_data$y)
  expect_gt(cov, 0.75)
})

test_that("conformal_lac validates inputs", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  expect_error(conformal_lac("bad", data$y, test_class_model_binary, x_new))
  expect_error(conformal_lac(data$x, data$y, test_class_model_binary, x_new,
                              alpha = 0))
})
