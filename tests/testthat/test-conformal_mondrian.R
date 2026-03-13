test_that("conformal_mondrian returns correct structure", {
  data <- make_regression_data(400, 3)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian(data$x, data$y, model = test_reg_model,
                                x_new = x_new, groups = groups,
                                groups_new = groups_new, seed = 42)

  expect_s3_class(result, "predictset_reg")
  expect_equal(result$method, "mondrian")
  expect_length(result$pred, 50)
  expect_true(all(result$lower <= result$upper))
  expect_true(all(c("high", "low") %in% names(result$group_quantiles)))
})

test_that("conformal_mondrian per-group quantiles differ", {
  set.seed(42)
  n <- 600
  x <- matrix(rnorm(n * 3), ncol = 3)
  groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
  # High group has much more noise
  y <- x[, 1] * 2 + ifelse(groups == "high", 3, 0.3) * rnorm(n)
  x_new <- matrix(rnorm(100 * 3), ncol = 3)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian(x, y, model = test_reg_model,
                                x_new = x_new, groups = groups,
                                groups_new = groups_new, seed = 42)

  # High-noise group should have larger quantile
  expect_gt(result$group_quantiles["high"], result$group_quantiles["low"])
})

test_that("conformal_mondrian falls back for unseen group", {
  data <- make_regression_data(200, 3)
  groups <- factor(rep("A", nrow(data$x)))
  x_new <- matrix(rnorm(10 * 3), ncol = 3)
  groups_new <- factor(rep("C", 10))

  suppressWarnings(
    result <- conformal_mondrian(data$x, data$y, model = test_reg_model,
                                  x_new = x_new, groups = groups,
                                  groups_new = groups_new, seed = 42)
  )
  expect_s3_class(result, "predictset_reg")
})

test_that("conformal_mondrian falls back for small group", {
  set.seed(42)
  n <- 50
  x <- matrix(rnorm(n * 3), ncol = 3)
  y <- rnorm(n)
  # Make groups so one has only 1 obs after split
  groups <- factor(c("A", rep("B", n - 1)))
  x_new <- matrix(rnorm(5 * 3), ncol = 3)
  groups_new <- factor(rep("A", 5))

  expect_warning(
    result <- conformal_mondrian(x, y, model = test_reg_model,
                                  x_new = x_new, groups = groups,
                                  groups_new = groups_new, seed = 42),
    "Falling back to pooled"
  )
  expect_s3_class(result, "predictset_reg")
})

test_that("conformal_mondrian_class returns correct structure", {
  data <- make_classification_data(400, 4, n_classes = 2)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(50 * 4), ncol = 4)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian_class(data$x, data$y,
                                      model = test_class_model_binary,
                                      x_new = x_new, groups = groups,
                                      groups_new = groups_new, seed = 42)

  expect_s3_class(result, "predictset_class")
  expect_equal(result$method, "mondrian")
  expect_length(result$sets, 50)
  sizes <- vapply(result$sets, length, integer(1))
  expect_true(all(sizes >= 1))
})

test_that("conformal_mondrian_class has per-group quantiles", {
  data <- make_classification_data(300, 4, n_classes = 2)
  groups <- factor(ifelse(data$x[, 1] > 0, "high", "low"))
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_mondrian_class(data$x, data$y,
                                      model = test_class_model_binary,
                                      x_new = x_new, groups = groups,
                                      groups_new = groups_new, seed = 42)

  expect_true(all(c("high", "low") %in% names(result$group_quantiles)))
})

test_that("conformal_mondrian validates groups length", {
  data <- make_regression_data(100, 3)
  groups <- factor(rep("A", 50))  # wrong length
  x_new <- matrix(rnorm(10 * 3), ncol = 3)
  groups_new <- factor(rep("A", 10))

  expect_error(
    conformal_mondrian(data$x, data$y, model = test_reg_model,
                        x_new = x_new, groups = groups,
                        groups_new = groups_new),
    "groups"
  )
})

test_that("conformal_mondrian validates groups_new length", {
  data <- make_regression_data(100, 3)
  groups <- factor(rep("A", 100))
  x_new <- matrix(rnorm(10 * 3), ncol = 3)
  groups_new <- factor(rep("A", 5))  # wrong length

  expect_error(
    conformal_mondrian(data$x, data$y, model = test_reg_model,
                        x_new = x_new, groups = groups,
                        groups_new = groups_new),
    "groups_new"
  )
})
