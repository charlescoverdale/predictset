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
