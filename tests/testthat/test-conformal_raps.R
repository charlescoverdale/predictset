test_that("conformal_raps returns correct structure", {
  data <- make_classification_data(300, 4, n_classes = 3)
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  clf <- make_multiclass_model()

  result <- conformal_raps(data$x, data$y, model = clf,
                            x_new = x_new, k_reg = 1, lambda = 0.01,
                            seed = 42)

  expect_s3_class(result, "predictset_class")
  expect_equal(result$method, "raps")
  expect_length(result$sets, 50)
})

test_that("conformal_raps sets contain valid classes", {
  data <- make_classification_data(200, 4, n_classes = 3)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  clf <- make_multiclass_model()

  result <- conformal_raps(data$x, data$y, model = clf,
                            x_new = x_new, seed = 42)

  for (s in result$sets) {
    expect_true(all(s %in% c("A", "B", "C")))
  }
})

test_that("conformal_raps stores k_reg and lambda", {
  data <- make_classification_data(200, 4, n_classes = 3)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  clf <- make_multiclass_model()

  result <- conformal_raps(data$x, data$y, model = clf,
                            x_new = x_new, k_reg = 2, lambda = 0.05,
                            seed = 42)

  expect_equal(result$k_reg, 2)
  expect_equal(result$lambda, 0.05)

  # predict should use stored k_reg/lambda
  x_new2 <- matrix(rnorm(5 * 4), ncol = 4)
  preds <- predict(result, newdata = x_new2)
  expect_length(preds$sets, 5)
})

test_that("conformal_raps achieves approximate coverage", {
  data <- make_classification_data(800, 4, n_classes = 3)
  test_data <- make_classification_data(300, 4, n_classes = 3, seed = 999)
  clf <- make_multiclass_model()

  result <- conformal_raps(data$x, data$y, model = clf,
                            x_new = test_data$x, alpha = 0.10, seed = 42)

  cov <- coverage(result, test_data$y)
  expect_gt(cov, 0.75)
})
