test_that("coverage.predictset_reg works correctly", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  cov <- coverage(result, y_new)
  expect_true(is.numeric(cov))
  expect_true(cov >= 0 && cov <= 1)
})

test_that("coverage validates length", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_error(coverage(result, rnorm(5)))
})

test_that("interval_width works", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  widths <- interval_width(result)
  expect_length(widths, 20)
  expect_true(all(widths >= 0))
})

test_that("interval_width rejects classification objects", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_error(interval_width(result))
})

test_that("set_size works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  sizes <- set_size(result)
  expect_length(sizes, 30)
  expect_true(all(sizes >= 1))
})

test_that("set_size rejects regression objects", {
  data <- make_regression_data(100, 3)
  x_new <- matrix(rnorm(20 * 3), ncol = 3)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  expect_error(set_size(result))
})

test_that("coverage.predictset_class works", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  y_new <- factor(sample(c("A", "B"), 30, replace = TRUE), levels = c("A", "B"))

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  cov <- coverage(result, y_new)
  expect_true(is.numeric(cov))
  expect_true(cov >= 0 && cov <= 1)
})

test_that("coverage_by_group returns correct structure", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(60 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(60, sd = 0.5)
  groups <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  cov_grp <- coverage_by_group(result, y_new, groups)

  expect_true(is.data.frame(cov_grp))
  expect_true(all(c("group", "coverage", "n", "target") %in% names(cov_grp)))
  expect_equal(nrow(cov_grp), 2)
  expect_true(all(cov_grp$coverage >= 0 & cov_grp$coverage <= 1))
  expect_equal(sum(cov_grp$n), 60)
})

test_that("coverage_by_group single group equals overall", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(50 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(50, sd = 0.5)
  groups <- factor(rep("all", 50))

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  cov_overall <- coverage(result, y_new)
  cov_grp <- coverage_by_group(result, y_new, groups)

  expect_equal(cov_grp$coverage, cov_overall)
})

test_that("coverage_by_group works for classification", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(40 * 4), ncol = 4)
  y_new <- factor(sample(c("A", "B"), 40, replace = TRUE), levels = c("A", "B"))
  groups <- factor(rep(c("G1", "G2"), 20))

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  cov_grp <- coverage_by_group(result, y_new, groups)
  expect_equal(nrow(cov_grp), 2)
})

test_that("coverage_by_bin returns correct structure", {
  data <- make_regression_data(200, 3)
  x_new <- matrix(rnorm(100 * 3), ncol = 3)
  y_new <- x_new[, 1] * 2 + x_new[, 2] + rnorm(100, sd = 0.5)

  result <- conformal_split(data$x, data$y, model = test_reg_model,
                             x_new = x_new, seed = 42)

  cov_bin <- coverage_by_bin(result, y_new, bins = 5)

  expect_true(is.data.frame(cov_bin))
  expect_true(all(c("bin", "coverage", "n", "mean_width") %in% names(cov_bin)))
  expect_equal(nrow(cov_bin), 5)
  expect_true(all(cov_bin$coverage >= 0 & cov_bin$coverage <= 1))
  expect_equal(sum(cov_bin$n), 100)
})

test_that("coverage_by_bin rejects classification objects", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)
  y_new <- factor(sample(c("A", "B"), 30, replace = TRUE))

  result <- conformal_lac(data$x, data$y, test_class_model_binary,
                           x_new = x_new, seed = 42)

  expect_error(coverage_by_bin(result, rnorm(30)))
})
