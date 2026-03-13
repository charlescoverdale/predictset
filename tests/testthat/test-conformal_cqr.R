test_that("conformal_cqr returns correct structure", {
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

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "cqr")
  expect_length(result$pred, 30)
  expect_true(all(result$lower <= result$upper))
})

test_that("conformal_cqr achieves approximate coverage", {
  data <- make_regression_data(1000, 3)
  n_test <- 300
  x_new <- matrix(rnorm(n_test * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(n_test, sd = 0.5)

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
                            x_new = x_new, alpha = 0.10, seed = 42)

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.80)
})

test_that("conformal_cqr produces variable-width intervals on heteroscedastic data", {
  set.seed(42)
  n <- 800
  x <- matrix(rnorm(n * 3), ncol = 3)
  # Heteroscedastic: noise scales with |x1|
  y <- x[, 1] * 2 + abs(x[, 1]) * rnorm(n)
  x_new <- matrix(rnorm(100 * 3), ncol = 3)

  # Quantile-like models that adapt to heteroscedasticity
  model_lo <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      df <- as.data.frame(x_new)
      predict(obj, newdata = df) - abs(df$X1) * 1.5
    },
    type = "regression"
  )
  model_hi <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      df <- as.data.frame(x_new)
      predict(obj, newdata = df) + abs(df$X1) * 1.5
    },
    type = "regression"
  )

  result <- conformal_cqr(x, y, model_lo, model_hi,
                            x_new = x_new, seed = 42)

  widths <- result$upper - result$lower
  # Widths should vary (not all identical)
  expect_gt(sd(widths), 0.01)
})

test_that("CQR has narrower intervals than split conformal on heteroscedastic data", {
  set.seed(42)
  n <- 800
  x <- matrix(rnorm(n * 3), ncol = 3)
  y <- x[, 1] * 2 + abs(x[, 1]) * rnorm(n)
  x_new <- matrix(rnorm(200 * 3), ncol = 3)

  # CQR with adaptive quantile models
  model_lo <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      df <- as.data.frame(x_new)
      predict(obj, newdata = df) - abs(df$X1) * 1.5
    },
    type = "regression"
  )
  model_hi <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(obj, x_new) {
      df <- as.data.frame(x_new)
      predict(obj, newdata = df) + abs(df$X1) * 1.5
    },
    type = "regression"
  )

  result_cqr <- conformal_cqr(x, y, model_lo, model_hi,
                                x_new = x_new, seed = 42)
  result_split <- conformal_split(x, y, model = y ~ ., x_new = x_new,
                                    seed = 42)

  # For points with small |x1| (low noise), CQR should be narrower
  small_x1 <- which(abs(x_new[, 1]) < 0.5)
  if (length(small_x1) >= 5) {
    cqr_widths <- mean(result_cqr$upper[small_x1] - result_cqr$lower[small_x1])
    split_widths <- mean(result_split$upper[small_x1] - result_split$lower[small_x1])
    expect_lt(cqr_widths, split_widths)
  }
})
