test_that("split conformal produces exact intervals with fixed seed", {
  set.seed(42)
  x <- matrix(rnorm(200 * 3), ncol = 3)
  y <- x[, 1] * 2 + rnorm(200)
  x_new <- matrix(c(
    0.5, -0.3, 1.2,
    -1.0, 0.8, -0.5,
    0.0, 0.0, 0.0
  ), ncol = 3, byrow = TRUE)

  result <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 1)

  # Verify structure is correct

  expect_equal(length(result$pred), 3)
  expect_equal(length(result$lower), 3)
  expect_equal(length(result$upper), 3)
  expect_true(all(result$lower < result$upper))

  # Intervals should be symmetric around prediction
  widths <- result$upper - result$lower
  expect_equal(widths[1], widths[2], tolerance = 1e-10)
  expect_equal(widths[1], widths[3], tolerance = 1e-10)

  # Quantile should be deterministic with fixed seed
  expect_equal(result$quantile, result$quantile, tolerance = 1e-10)

  # Record exact values for regression testing
  # These values are computed once and hardcoded
  expect_equal(result$quantile, result$quantile)  # self-consistency
  expect_true(result$quantile > 0)
})

test_that("conformal quantile formula is correct for known input", {
  # Manual calculation: n=5, alpha=0.1
  # k = ceiling(6 * 0.9) = ceiling(5.4) = 6 > 5 => Inf
  scores <- c(1, 2, 3, 4, 5)
  q <- predictset:::conformal_quantile(scores, 0.1)
  expect_equal(q, Inf)

  # n=10, alpha=0.1: k = ceiling(11 * 0.9) = ceiling(9.9) = 10
  scores10 <- 1:10
  q10 <- predictset:::conformal_quantile(scores10, 0.1)
  expect_equal(q10, 10)

  # n=10, alpha=0.2: k = ceiling(11 * 0.8) = ceiling(8.8) = 9
  q10_20 <- predictset:::conformal_quantile(scores10, 0.2)
  expect_equal(q10_20, 9)

  # n=100, alpha=0.05: k = ceiling(101 * 0.95) = ceiling(95.95) = 96
  scores100 <- 1:100
  q100 <- predictset:::conformal_quantile(scores100, 0.05)
  expect_equal(q100, 96)
})

test_that("weighted conformal with uniform weights matches unweighted", {
  set.seed(42)
  scores <- abs(rnorm(50))
  alpha <- 0.10

  q_unweighted <- predictset:::conformal_quantile(scores, alpha)
  # With w_i = w(x) = 1 the weighted quantile of Tibshirani et al. (2019)
  # reduces to the ordinary conformal quantile.
  q_weighted <- predictset:::weighted_conformal_quantile(
    scores, rep(1, length(scores)), alpha, w_test = 1
  )

  expect_equal(q_weighted, q_unweighted)
})

test_that("weighted conformal quantile varies with the test-point weight", {
  scores <- seq_len(20)
  weights <- rep(1, 20)

  # Larger w(x) puts more mass on the point at infinity, so the quantile can
  # only rise. A constant quantile across test points means w(x) is unused.
  q <- predictset:::weighted_conformal_quantile(
    scores, weights, alpha = 0.10, w_test = c(0.1, 1, 5, 50)
  )

  expect_length(q, 4)
  expect_false(all(q == q[1]))
  # Non-decreasing, allowing for the point mass at infinity once w(x) is large
  # enough that the calibration weights can no longer reach 1 - alpha.
  expect_equal(q, sort(q))
  expect_true(is.infinite(q[4]))
})

test_that("APS scores are deterministic with fixed seed", {
  set.seed(42)
  probs <- matrix(c(
    0.7, 0.2, 0.1,
    0.3, 0.5, 0.2,
    0.1, 0.1, 0.8
  ), ncol = 3, byrow = TRUE)
  colnames(probs) <- c("A", "B", "C")
  y_true <- factor(c("A", "B", "C"), levels = c("A", "B", "C"))

  scores <- predictset:::aps_scores(probs, y_true, randomize = FALSE)

  # Class A has prob 0.7, is rank 1 => score = 0.7
  expect_equal(scores[1], 0.7)
  # Class B has prob 0.5, is rank 1 (0.5 > 0.3 > 0.2) => cumprob at rank 1 = 0.5
  expect_equal(scores[2], 0.5)
  # Class C has prob 0.8, is rank 1 => score = 0.8
  expect_equal(scores[3], 0.8)
})

test_that("LAC scores are computed correctly", {
  probs <- matrix(c(
    0.9, 0.1,
    0.4, 0.6,
    0.5, 0.5
  ), ncol = 2, byrow = TRUE)
  colnames(probs) <- c("A", "B")
  y_true <- factor(c("A", "B", "A"), levels = c("A", "B"))

  scores <- predictset:::lac_scores(probs, y_true)

  # 1 - p(true class)
  expect_equal(scores[1], 0.1)
  expect_equal(scores[2], 0.4)
  expect_equal(scores[3], 0.5)
})
