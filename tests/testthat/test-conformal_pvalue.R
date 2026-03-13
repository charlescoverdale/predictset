test_that("conformal_pvalue returns correct values", {
  scores <- c(1, 2, 3, 4, 5)
  new_scores <- c(0, 3, 6)

  pvals <- conformal_pvalue(scores, new_scores)

  expect_length(pvals, 3)
  # score = 0: all 5 >= 0, so (1 + 5) / 6 = 1.0

  expect_equal(pvals[1], 1.0)
  # score = 3: 3 scores >= 3 (3,4,5), so (1 + 3) / 6 = 4/6
  expect_equal(pvals[2], 4 / 6)
  # score = 6: 0 scores >= 6, so (1 + 0) / 6 = 1/6
  expect_equal(pvals[3], 1 / 6)
})

test_that("conformal_pvalue validates inputs", {
  expect_error(conformal_pvalue(c(), c(1)), "non-empty")
  expect_error(conformal_pvalue(c(1), c()), "non-empty")
  expect_error(conformal_pvalue("bad", c(1)), "numeric")
})

test_that("conformal_aci returns correct structure", {
  set.seed(42)
  n <- 100
  y_true <- rnorm(n)
  y_pred <- c(0, y_true[-n])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  expect_type(result, "list")
  expect_length(result$lower, n)
  expect_length(result$upper, n)
  expect_length(result$covered, n)
  expect_length(result$alphas, n)
  expect_true(result$coverage >= 0 && result$coverage <= 1)
})

test_that("conformal_aci first interval is infinite", {
  y_true <- rnorm(10)
  y_pred <- rnorm(10)

  result <- conformal_aci(y_pred, y_true)

  expect_equal(result$lower[1], -Inf)
  expect_equal(result$upper[1], Inf)
  expect_true(result$covered[1])
})

test_that("conformal_aci validates inputs", {
  expect_error(conformal_aci(c(1, 2), c(1)), "same length")
  expect_error(conformal_aci(c(1), c(1)), "At least 2")
  expect_error(conformal_aci(c(1, 2), c(1, 2), gamma = -1), "positive")
})

test_that("conformal_aci adapts alpha over time", {
  set.seed(42)
  n <- 200
  y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)
  y_pred <- c(0, y_true[-n])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  # Alpha should change over time (not stay constant)
  expect_false(all(result$alphas == result$alphas[1]))
})
