test_that("predict.predictset_reg works", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  x_new2 <- matrix(rnorm(10 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 10)
  expect_named(preds, c("pred", "lower", "upper"))
  expect_true(all(preds$lower <= preds$upper))
})

test_that("predict.predictset_reg works for CQR", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

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

  x_new2 <- matrix(rnorm(5 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_equal(nrow(preds), 5)
  expect_true(all(preds$lower <= preds$upper))
})

test_that("predict.predictset_class works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y,
                           model = test_class_model_binary,
                           x_new = x_new, seed = 42)

  x_new2 <- matrix(rnorm(10 * 4), ncol = 4)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "predictset_class")
  expect_length(preds$sets, 10)
})
