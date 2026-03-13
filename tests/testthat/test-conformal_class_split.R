test_that("conformal_class_split returns correct structure", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_class_split(data$x, data$y,
                                   model = test_class_model_binary,
                                   x_new = x_new, seed = 42)

  expect_s3_class(result, "predictset_class")
  expect_equal(result$method, "split")
  expect_length(result$sets, 30)
  expect_true(all(vapply(result$sets, function(s) length(s) >= 1, logical(1))))
})

test_that("conformal_class_split prediction sets contain valid classes", {
  data <- make_classification_data(200, 4, n_classes = 2)
  x_new <- matrix(rnorm(30 * 4), ncol = 4)

  result <- conformal_class_split(data$x, data$y,
                                   model = test_class_model_binary,
                                   x_new = x_new, seed = 42)

  for (s in result$sets) {
    expect_true(all(s %in% result$classes))
  }
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

  result <- conformal_class_split(data$x, data$y,
                                   model = test_class_model_binary,
                                   x_new = x_new, alpha = 0.10, seed = 42)

  cov <- coverage(result, y_new)
  expect_gt(cov, 0.75)
})
