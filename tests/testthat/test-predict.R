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

test_that("predict.predictset_reg works for Mondrian with group quantiles", {
  set.seed(42)
  n <- 600
  x <- matrix(rnorm(n * 3), ncol = 3)
  groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
  y <- x[, 1] * 2 + ifelse(groups == "high", 3, 0.3) * rnorm(n)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian(x, y, model = test_reg_model,
                                x_new = x_new, groups = groups,
                                groups_new = groups_new, seed = 42)

  # Predict on new data with groups
  x_new2 <- matrix(rnorm(20 * 3), ncol = 3)
  groups_new2 <- factor(ifelse(x_new2[, 1] > 0, "high", "low"))
  preds <- predict(result, newdata = x_new2, groups_new = groups_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 20)
  expect_true(all(preds$lower <= preds$upper))

  # Intervals should differ by group (high-noise group gets wider intervals)
  high_idx <- which(groups_new2 == "high")
  low_idx <- which(groups_new2 == "low")
  if (length(high_idx) > 0 && length(low_idx) > 0) {
    high_widths <- mean(preds$upper[high_idx] - preds$lower[high_idx])
    low_widths <- mean(preds$upper[low_idx] - preds$lower[low_idx])
    expect_gt(high_widths, low_widths)
  }
})

test_that("predict.predictset_reg Mondrian errors without groups_new", {
  data <- make_regression_data(200, 3)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(20 * 3), ncol = 3)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian(data$x, data$y, model = test_reg_model,
                                x_new = x_new, groups = groups,
                                groups_new = groups_new, seed = 42)

  x_new2 <- matrix(rnorm(5 * 3), ncol = 3)
  expect_error(predict(result, newdata = x_new2), "groups_new")
})

test_that("predict.predictset_class works for Mondrian with group quantiles", {
  data <- make_classification_data(400, 4, n_classes = 2)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian_class(data$x, data$y,
                                      model = test_class_model_binary,
                                      x_new = x_new, groups = groups,
                                      groups_new = groups_new, seed = 42)

  # Predict on new data with groups
  x_new2 <- matrix(rnorm(20 * 4), ncol = 4)
  groups_new2 <- factor(ifelse(x_new2[, 1] > 0, "high", "low"))
  preds <- predict(result, newdata = x_new2, groups_new = groups_new2)

  expect_s3_class(preds, "predictset_class")
  expect_length(preds$sets, 20)
  sizes <- vapply(preds$sets, length, integer(1))
  expect_true(all(sizes >= 1))
})

test_that("predict.predictset_class Mondrian errors without groups_new", {
  data <- make_classification_data(300, 4, n_classes = 2)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian_class(data$x, data$y,
                                      model = test_class_model_binary,
                                      x_new = x_new, groups = groups,
                                      groups_new = groups_new, seed = 42)

  x_new2 <- matrix(rnorm(5 * 4), ncol = 4)
  expect_error(predict(result, newdata = x_new2), "groups_new")
})

test_that("predict.predictset_reg works for weighted conformal", {
  data <- make_regression_data(400, 3)
  x_new <- matrix(rnorm(30 * 3), ncol = 3)
  weights <- rep(1, nrow(data$x))

  result <- conformal_weighted(data$x, data$y, model = test_reg_model,
                                x_new = x_new, weights = weights, seed = 42)

  x_new2 <- matrix(rnorm(10 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 10)
  expect_true(all(preds$lower <= preds$upper))
})

test_that("predict.predictset_reg works for Jackknife+", {
  data <- make_regression_data(50, 3)
  x_new <- matrix(rnorm(10 * 3), ncol = 3)

  result <- conformal_jackknife(data$x, data$y, model = test_reg_model,
                                 x_new = x_new, seed = 42)

  x_new2 <- matrix(rnorm(5 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 5)
  expect_true(all(preds$lower <= preds$upper))
})

test_that("predict.predictset_reg works for CV+", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(10 * 3), ncol = 3)

  result <- conformal_cv(data$x, data$y, model = test_reg_model,
                          x_new = x_new, n_folds = 5, seed = 42)

  x_new2 <- matrix(rnorm(5 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 5)
  expect_true(all(preds$lower <= preds$upper))
})

test_that("predict works with data.frame inputs", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)
  colnames(x_new) <- paste0("X", 1:3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  x_new2 <- as.data.frame(matrix(rnorm(5 * 3), ncol = 3))
  names(x_new2) <- paste0("X", 1:3)
  preds <- predict(result, newdata = x_new2)

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 5)
})
