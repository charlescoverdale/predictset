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

test_that("conformal_aci returns predictset_aci class", {
  set.seed(42)
  n <- 100
  y_true <- rnorm(n)
  y_pred <- c(0, y_true[-n])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  expect_s3_class(result, "predictset_aci")
  expect_length(result$lower, n)
  expect_length(result$upper, n)
  expect_length(result$covered, n)
  expect_length(result$alphas, n)
  expect_true(result$coverage >= 0 && result$coverage <= 1)
  expect_equal(result$alpha, 0.10)
  expect_equal(result$gamma, 0.01)
  expect_equal(result$n, n)
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

test_that("conformal_aci achieves long-run coverage near target", {
  set.seed(123)
  n <- 500
  y_true <- rnorm(n)
  y_pred <- c(0, y_true[-n])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.005)

  # Long-run coverage should be close to 1 - alpha = 0.90
  expect_gt(result$coverage, 0.80)
  expect_lt(result$coverage, 0.99)
})

test_that("conformal_aci larger gamma adapts faster", {
  set.seed(42)
  n <- 300
  # Distribution shift at midpoint
  y_true <- c(rnorm(150, mean = 0), rnorm(150, mean = 5))
  y_pred <- c(0, y_true[-n])

  result_slow <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.001)
  result_fast <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.05)

  # Faster gamma should have more alpha variation
  alpha_var_slow <- var(result_slow$alphas)
  alpha_var_fast <- var(result_fast$alphas)
  expect_gt(alpha_var_fast, alpha_var_slow)
})

test_that("conformal_aci responds to distribution shift by widening", {
  set.seed(42)
  n <- 400
  # Stable period then variance increase causes miscoverage
  y_true <- c(rnorm(200, mean = 0, sd = 1), rnorm(200, mean = 0, sd = 2))
  y_pred <- c(0, y_true[-n])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  alpha_early <- mean(result$alphas[150:200])
  alpha_late <- mean(result$alphas[350:400])

  # Gibbs and Candes (2021), Eq. 2: alpha_{t+1} = alpha_t + gamma(alpha - err_t).
  # A run of misses drives alpha_t DOWN, which raises the quantile and widens
  # the interval. The reverse sign is positive feedback: misses would tighten
  # the interval, causing more misses, and alpha_t runs to a clip boundary.
  expect_lt(alpha_late, alpha_early)

  # The feedback must be stable, not a runaway to the [0.001, 0.999] clips.
  expect_gt(min(result$alphas), 0.001)
  expect_lt(max(result$alphas), 0.999)
})

test_that("conformal_aci matches the Gibbs and Candes update exactly", {
  set.seed(11)
  n <- 300
  y_true <- c(rnorm(150), rnorm(150, mean = 6))
  y_pred <- c(0, y_true[-n])
  alpha <- 0.10
  gamma <- 0.02

  result <- conformal_aci(y_pred, y_true, alpha = alpha, gamma = gamma)

  # Independent transcription of the published recursion.
  cq <- function(s, a) {
    k <- ceiling((length(s) + 1) * (1 - a))
    if (k > length(s)) Inf else sort(s)[k]
  }
  res_v <- numeric(n)
  alphas <- numeric(n)
  covered <- logical(n)
  at <- alpha
  for (t in seq_len(n)) {
    alphas[t] <- at
    q <- if (t == 1L) Inf else cq(res_v[seq_len(t - 1L)], at)
    covered[t] <- y_true[t] >= y_pred[t] - q && y_true[t] <= y_pred[t] + q
    res_v[t] <- abs(y_true[t] - y_pred[t])
    at <- max(0.001, min(0.999, at + gamma * (alpha - as.numeric(!covered[t]))))
  }

  expect_equal(result$alphas, alphas)
  expect_equal(result$covered, covered)
})

test_that("conformal_aci holds long-run coverage under a level shift", {
  set.seed(7)
  n <- 1000
  y_true <- c(rnorm(500), rnorm(500, mean = 20))
  y_pred <- rep(0, n)

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.02)

  # A sign-reversed update collapses this to ~0.60 or inflates it to 1.00
  # with infinite intervals; the published update tracks the target.
  expect_gt(result$coverage, 0.85)
  expect_lt(result$coverage, 0.95)
})

test_that("print.predictset_aci works", {
  set.seed(42)
  y_true <- rnorm(50)
  y_pred <- c(0, y_true[-50])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  expect_no_error(print(result))
})

test_that("plot.predictset_aci works", {
  set.seed(42)
  y_true <- rnorm(50)
  y_pred <- c(0, y_true[-50])

  result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)

  ret <- plot(result)
  expect_s3_class(ret, "predictset_aci")
})
