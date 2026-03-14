test_that("conformal_class_split emits deprecation warning", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  expect_warning(
    result <- conformal_class_split(data$x, data$y,
                                     model = test_class_model_binary,
                                     x_new = x_new, seed = 42),
    "conformal_lac"
  )

  expect_s3_class(result, "predictset_class")
  expect_equal(result$method, "lac")
  expect_length(result$sets, 30)
})

test_that("conformal_class_split produces identical output to conformal_lac", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  suppressWarnings(
    result_split <- conformal_class_split(data$x, data$y,
                                           model = test_class_model_binary,
                                           x_new = x_new, seed = 42)
  )
  result_lac <- conformal_lac(data$x, data$y,
                               model = test_class_model_binary,
                               x_new = x_new, seed = 42)

  expect_equal(result_split$sets, result_lac$sets)
  expect_equal(result_split$quantile, result_lac$quantile)
  expect_equal(result_split$scores, result_lac$scores)
})

test_that("conformal_class_split achieves approximate coverage", {
  data <- make_classification_data(500, 4, n_classes = 2)
  n_test <- 200
  set.seed(999)
  x_new <- matrix(rnorm(n_test * 4), ncol = 4)
  probs_new <- exp(x_new[, 1:2])
  probs_new <- probs_new / rowSums(probs_new)
  y_new <- factor(
    apply(probs_new, 1, function(p) sample(c("A", "B"), 1, prob = p)),
    levels = c("A", "B")
  )

  suppressWarnings(
    result <- conformal_class_split(data$x, data$y,
                                     model = test_class_model_binary,
                                     x_new = x_new, alpha = 0.10, seed = 42)
  )

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.75)
})
