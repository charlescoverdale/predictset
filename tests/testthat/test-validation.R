test_that("validate_x rejects NA matrix", {
  x <- matrix(c(1, NA, 3, 4), ncol = 2)
  expect_error(
    predictset:::validate_x(x),
    "must not contain NA"
  )
})

test_that("validate_x rejects NaN matrix", {
  x <- matrix(c(1, NaN, 3, 4), ncol = 2)
  expect_error(
    predictset:::validate_x(x),
    "must not contain"
  )
})

test_that("validate_x rejects Inf matrix", {
  x <- matrix(c(1, Inf, 3, 4), ncol = 2)
  expect_error(
    predictset:::validate_x(x),
    "must not contain NaN or Inf"
  )
})

test_that("validate_y_reg rejects NA", {
  expect_error(
    predictset:::validate_y_reg(c(1, NA, 3)),
    "must not contain NA"
  )
})

test_that("validate_y_reg rejects NaN", {
  expect_error(
    predictset:::validate_y_reg(c(1, NaN, 3)),
    "must not contain"
  )
})

test_that("validate_y_reg rejects Inf", {
  expect_error(
    predictset:::validate_y_reg(c(1, Inf, 3)),
    "must not contain NaN or Inf"
  )
})

test_that("validate_y_class rejects NA factor", {
  expect_error(
    predictset:::validate_y_class(factor(c("A", NA, "B"))),
    "must not contain NA"
  )
})

test_that("validate_y_reg warns on constant y", {
  expect_warning(
    predictset:::validate_y_reg(rep(5, 10)),
    "only one unique value"
  )
})

test_that("constant y produces valid output structure", {
  x <- matrix(rnorm(60), ncol = 3)
  y <- rep(5, 20)
  x_new <- matrix(rnorm(15), ncol = 3)

  expect_warning(
    result <- conformal_split(x, y, model = test_reg_model,
                               x_new = x_new, seed = 42),
    "only one unique value"
  )
  expect_s3_class(result, "predictset_reg")
  expect_length(result$pred, 5)
})

test_that("validate_x_new catches column mismatch", {
  x <- matrix(rnorm(30), ncol = 3)
  x_new <- matrix(rnorm(10), ncol = 2)

  expect_error(
    predictset:::validate_x_new(x, x_new),
    "must have 3 column"
  )
})

test_that("validate_probs_colnames catches missing levels", {
  probs <- matrix(c(0.5, 0.5, 0.3, 0.7), ncol = 2)
  colnames(probs) <- c("A", "B")
  y <- factor(c("A", "C"), levels = c("A", "B", "C"))

  expect_error(
    predictset:::validate_probs_colnames(probs, y),
    "missing columns"
  )
})

test_that("validate_probs_colnames catches single column", {
  probs <- matrix(c(0.5, 0.3), ncol = 1)
  colnames(probs) <- "A"
  y <- factor("A")

  expect_error(
    predictset:::validate_probs_colnames(probs, y),
    "at least 2 columns"
  )
})

test_that("lac_scores errors on unseen class labels", {
  probs <- matrix(c(0.5, 0.5, 0.5, 0.5), ncol = 2)
  colnames(probs) <- c("A", "B")
  y <- factor(c("A", "C"), levels = c("A", "B", "C"))

  expect_error(
    predictset:::lac_scores(probs, y),
    "not found in probability matrix"
  )
})

test_that("aps_scores handles unseen class by assigning worst score", {
  probs <- matrix(c(0.7, 0.3, 0.4, 0.6), ncol = 2, byrow = TRUE)
  colnames(probs) <- c("A", "B")
  # "C" is not in the probability matrix
  y <- factor(c("A", "C"), levels = c("A", "B", "C"))

  scores <- predictset:::aps_scores(probs, y)
  expect_equal(scores[2], 1.0)
})

test_that("raps_scores handles unseen class by assigning worst score", {
  probs <- matrix(c(0.7, 0.3, 0.4, 0.6), ncol = 2, byrow = TRUE)
  colnames(probs) <- c("A", "B")
  y <- factor(c("A", "C"), levels = c("A", "B", "C"))

  scores <- predictset:::raps_scores(probs, y, k_reg = 1, lambda = 0.1)
  expect_gt(scores[2], 1.0)
})

test_that("column dimension check works end-to-end", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20), ncol = 2)

  expect_error(
    conformal_split(data$x, data$y, model = test_reg_model,
                     x_new = x_new, seed = 42),
    "must have 3 column"
  )
})

test_that("negative scale model triggers warning", {
  x <- matrix(rnorm(200 * 3), ncol = 3)
  y <- x[, 1] * 2 + rnorm(200)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  neg_scale <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(object, x_new) {
      rep(-1, nrow(x_new))
    },
    type = "regression"
  )

  expect_warning(
    conformal_split(x, y, model = test_reg_model, x_new = x_new,
                     score_type = "normalized", scale_model = neg_scale,
                     seed = 42),
    "negative prediction"
  )
})
