test_that("plot.predictset_reg produces a plot and returns invisibly", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = y ~ ., x_new = x_new,
                             seed = 42)

  # Should return the object invisibly
  ret <- plot(result)
  expect_s3_class(ret, "predictset_reg")
})

test_that("plot.predictset_reg respects max_points", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(300 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = y ~ ., x_new = x_new,
                             seed = 42)

  # Should not error with max_points < n

  expect_no_error(plot(result, max_points = 50))
})

test_that("plot.predictset_class produces a plot and returns invisibly", {
  data <- make_classification_data(300, 4, n_classes = 2)
  x_new <- matrix(rnorm(50 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, model = test_class_model_binary,
                           x_new = x_new, seed = 42)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_class")
})

test_that("plot.predictset_reg handles CQR method", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)

  model_lo <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      predict(obj, newdata = as.data.frame(x_new)) - 1.5
    },
    type = "regression"
  )
  model_hi <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      predict(obj, newdata = as.data.frame(x_new)) + 1.5
    },
    type = "regression"
  )

  result <- conformal_cqr(data$x, data$y, model_lo, model_hi,
                            x_new = x_new, seed = 42)

  expect_no_error(plot(result))
})

test_that("plot.predictset_class handles APS method", {
  data <- make_classification_data(300, 4, n_classes = 2)
  x_new <- matrix(rnorm(50 * 4), ncol = 4)

  result <- conformal_aps(data$x, data$y, model = test_class_model_binary,
                           x_new = x_new, seed = 42)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_class")
})

test_that("plot.predictset_reg handles Mondrian method (no NA title)", {
  data <- make_regression_data(400, 3)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian(data$x, data$y, model = test_reg_model,
                                x_new = x_new, groups = groups,
                                groups_new = groups_new, seed = 42)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_reg")
})

test_that("plot.predictset_reg handles weighted method (no NA title)", {
  data <- make_regression_data(400, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)
  weights <- rep(1, nrow(data$x))

  result <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                x_new = x_new, weights = weights, seed = 42)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_reg")
})

test_that("plot.predictset_class handles Mondrian method (no NA title)", {
  data <- make_classification_data(400, 4, n_classes = 2)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian_class(data$x, data$y,
                                      model = test_class_model_binary,
                                      x_new = x_new, groups = groups,
                                      groups_new = groups_new, seed = 42)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_class")
})
