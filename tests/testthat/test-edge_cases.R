test_that("small n with cal_fraction = 0.5 works", {
  set.seed(99)
  x <- matrix(rnorm(5 * 2), ncol = 2)
  y <- rnorm(5)
  x_new <- matrix(rnorm(3 * 2), ncol = 2)

  result <- conformal_split(x, y, model = test_reg_model,
                             x_new = x_new, cal_fraction = 0.5, seed = 42)
  expect_s3_class(result, "predictset_reg")
  expect_length(result$pred, 3)
})

test_that("alpha = 0.001 produces wide intervals", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, alpha = 0.001, seed = 42)
  expect_true(all(result$lower <= result$upper))
  widths <- result$upper - result$lower
  expect_true(all(widths > 0))
})

test_that("alpha = 0.99 produces narrow intervals", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, alpha = 0.99, seed = 42)
  expect_true(all(result$lower <= result$upper))
})

test_that("identical calibration scores (ties) work", {
  x <- matrix(rnorm(100 * 3), ncol = 3)
  y <- rep(0, 100)  # all identical -> all residuals will be equal
  x_new <- matrix(rnorm(10 * 3), ncol = 3)

  # Use a model that always predicts 0
  const_model <- make_model(
    train_fun = function(x, y) NULL,
    predict_fun = function(object, x_new) rep(0, nrow(x_new)),
    type = "regression"
  )

  expect_warning(
    result <- conformal_split(x, y, model = const_model,
                               x_new = x_new, seed = 42),
    "only one unique value"
  )
  expect_s3_class(result, "predictset_reg")
})

test_that("perfect prediction (all residuals = 0) works", {
  set.seed(42)
  x <- matrix(1:20, ncol = 1)
  y <- x[, 1] * 2  # perfect linear
  x_new <- matrix(21:25, ncol = 1)

  result <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 42)
  expect_s3_class(result, "predictset_reg")
  expect_true(all(result$lower <= result$upper))
})

test_that("RAPS with lambda = 0 behaves like APS", {
  data <- make_classification_data(300, 4, n_classes = 3)
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  clf <- make_multiclass_model()

  result_aps <- conformal_aps(data$x, data$y, model = clf,
                               x_new = x_new, seed = 42)
  result_raps <- conformal_raps(data$x, data$y, model = clf,
                                 x_new = x_new, k_reg = 1, lambda = 0,
                                 seed = 42)

  expect_equal(result_aps$scores, result_raps$scores)
  expect_equal(result_aps$quantile, result_raps$quantile)
})

test_that("RAPS with k_reg > number of classes works", {
  data <- make_classification_data(300, 4, n_classes = 3)
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  clf <- make_multiclass_model()

  result <- conformal_raps(data$x, data$y, model = clf,
                            x_new = x_new, k_reg = 100, lambda = 0.01,
                            seed = 42)
  expect_s3_class(result, "predictset_class")
  expect_length(result$sets, 50)
})

test_that("CV+ with n_folds = 2 works", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(10 * 3), ncol = 3)

  result <- conformal_cv(data$x, data$y, model = test_reg_model,
                          x_new = x_new, n_folds = 2, seed = 42)
  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "cv_plus")
})

test_that("jackknife with small n runs without info message", {
  # Verify that n <= 500 doesn't produce an informational message
  data <- make_regression_data(20, 2)
  x_new <- matrix(rnorm(5 * 2), ncol = 2)

  # Should not produce any message for small n
  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                 x_new = x_new, seed = 42)
  expect_s3_class(result, "predictset_reg")
})
