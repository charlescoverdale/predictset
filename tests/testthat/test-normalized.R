test_that("normalized conformal produces variable-width intervals", {
  set.seed(42)
  n <- 500
  x <- matrix(rnorm(n * 3), ncol = 3)
  # Heteroscedastic noise: variance depends on x[,1]
  y <- x[, 1] * 2 + rnorm(n, sd = abs(x[, 1]) + 0.1)
  x_new <- matrix(rnorm(100 * 3), ncol = 3)

  result <- conformal_split(x, y, model = test_reg_model, x_new = x_new,
                             score_type = "normalized", seed = 42)

  expect_s3_class(result, "predictset_reg")
  widths <- result$upper - result$lower
  # Normalized intervals should have varying widths
  expect_gt(sd(widths), 0)
  # Should have score_type field

  expect_equal(result$score_type, "normalized")
})

test_that("normalized conformal predict works on new data", {
  set.seed(42)
  n <- 200
  x <- matrix(rnorm(n * 3), ncol = 3)
  y <- x[, 1] * 2 + rnorm(n, sd = abs(x[, 1]) + 0.1)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)

  result <- conformal_split(x, y, model = test_reg_model, x_new = x_new,
                             score_type = "normalized", seed = 42)

  x_new2 <- matrix(rnorm(10 * 3), ncol = 3)
  preds <- predict(result, newdata = x_new2)

  expect_equal(nrow(preds), 10)
  widths <- preds$upper - preds$lower
  expect_gt(sd(widths), 0)
  expect_true(all(preds$lower <= preds$upper))
})

test_that("absolute conformal produces constant-width intervals", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, score_type = "absolute", seed = 42)

  widths <- result$upper - result$lower
  expect_equal(length(unique(round(widths, 10))), 1)
})
