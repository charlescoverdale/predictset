# Regression tests for the defects found in the 0.3.2 audit. Each one failed
# before the corresponding fix, so each is pinned here.

# --- The model argument must actually be used ---------------------------------

test_that("a formula's right-hand side is honoured, not replaced by y ~ .", {
  set.seed(2)
  n <- 300
  x <- data.frame(a = rnorm(n), b = rnorm(n), junk = rnorm(n))
  y <- 5 * x$a + rnorm(n, sd = 0.2)

  restricted <- conformal_split(x, y, model = y ~ a, x_new = x[1:5, ],
                                seed = 1)
  full <- conformal_split(x, y, model = y ~ ., x_new = x[1:5, ], seed = 1)

  expect_named(coef(restricted$fitted_model), c("(Intercept)", "a"))
  expect_length(coef(full$fitted_model), 4L)
})

test_that("a fitted model's specification survives refitting", {
  set.seed(3)
  n <- 300
  x <- matrix(rnorm(n * 2), ncol = 2)
  colnames(x) <- c("v1", "v2")
  y <- 3 * x[, 1]^2 + rnorm(n)
  fit <- lm(y ~ poly(v1, 3) + v2, data = data.frame(y = y, x))

  result <- conformal_split(x, y, model = fit, x_new = x[1:5, ], seed = 1)

  expect_equal(formula(result$fitted_model), formula(fit))
  # Refitted on the training split, so not the same object as the input.
  expect_false(identical(coef(result$fitted_model), coef(fit)))
})

test_that("a formula naming an absent variable errors rather than silently refitting", {
  set.seed(4)
  x <- matrix(rnorm(100), ncol = 2)
  y <- rnorm(50)

  expect_error(
    conformal_split(x, y, model = y ~ nonexistent, x_new = x[1:3, ]),
    "not present in"
  )
})

test_that("an object with no training path is rejected with a pointer to make_model", {
  x <- matrix(rnorm(100), ncol = 2)
  y <- rnorm(50)

  expect_error(
    conformal_split(x, y, model = list(a = 1), x_new = x[1:3, ]),
    "make_model"
  )
})

test_that("a fitted glm works with the classification methods", {
  set.seed(5)
  n <- 300
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- factor(ifelse(plogis(x[, 1]) > runif(n), "A", "B"))
  g <- glm(y ~ ., data = data.frame(y = y, x), family = "binomial")

  result <- conformal_lac(x, y, model = g, x_new = x[1:20, ], seed = 1)

  expect_s3_class(result, "predictset_class")
  expect_equal(result$classes, c("A", "B"))
  expect_true(all(unlist(result$sets) %in% c("A", "B")))
})

# --- Jackknife+ and CV+ order-statistic convention ----------------------------

test_that("Jackknife+ returns infinite bounds when the quantile index exceeds n", {
  set.seed(1)
  n <- 6
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- x[, 1] + rnorm(n)
  x_new <- matrix(rnorm(3 * 2), ncol = 2)

  # alpha = 0.10 needs n >= 9; below that Barber et al. (2021) define the
  # bounds as -Inf / +Inf. Clamping to the extreme order statistic instead
  # produces an interval narrower than the estimator is defined to be.
  result <- conformal_jackknife(x, y, model = y ~ ., x_new = x_new,
                                 alpha = 0.10)

  expect_true(all(is.infinite(result$lower)))
  expect_true(all(is.infinite(result$upper)))
})

test_that("CV+ returns infinite bounds when the quantile index exceeds n", {
  set.seed(1)
  n <- 8
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- x[, 1] + rnorm(n)
  x_new <- matrix(rnorm(3 * 2), ncol = 2)

  result <- conformal_cv(x, y, model = y ~ ., x_new = x_new, n_folds = 4,
                          alpha = 0.10)

  expect_true(all(is.infinite(result$upper)))
})

test_that("Jackknife+ bounds are finite once n is large enough", {
  set.seed(1)
  n <- 30
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- x[, 1] + rnorm(n)
  x_new <- matrix(rnorm(3 * 2), ncol = 2)

  result <- conformal_jackknife(x, y, model = y ~ ., x_new = x_new,
                                 alpha = 0.10)

  expect_true(all(is.finite(result$lower)))
  expect_true(all(is.finite(result$upper)))
})

test_that("predict() reproduces the Jackknife+ intervals it was fitted with", {
  set.seed(6)
  n <- 40
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- x[, 1] + rnorm(n)
  x_new <- matrix(rnorm(5 * 2), ncol = 2)

  result <- conformal_jackknife(x, y, model = y ~ ., x_new = x_new,
                                 alpha = 0.10)
  again <- predict(result, newdata = x_new)

  expect_equal(again$lower, result$lower)
  expect_equal(again$upper, result$upper)
})

# --- APS / RAPS randomisation -------------------------------------------------

test_that("APS is not degenerate: sets are smaller than the full label set", {
  # Oracle probabilities, so any inefficiency is the method's, not the model's.
  truep <- function(xm) {
    L <- cbind(0, 1.2 * xm[, 1], 1.2 * xm[, 2], 0.8 * (xm[, 1] - xm[, 2]))
    e <- exp(L)
    p <- e / rowSums(e)
    colnames(p) <- LETTERS[1:4]
    p
  }
  oracle <- make_model(function(x, y) NULL, function(o, xn) truep(xn),
                       "classification")

  set.seed(9)
  n <- 2000
  x <- matrix(rnorm(n * 2), ncol = 2)
  P <- truep(x)
  y <- factor(LETTERS[1:4][apply(P, 1, function(p) sample.int(4, 1, prob = p))],
              levels = LETTERS[1:4])
  x_new <- matrix(rnorm(1000 * 2), ncol = 2)
  Pn <- truep(x_new)
  y_new <- factor(LETTERS[1:4][apply(Pn, 1, function(p) sample.int(4, 1, prob = p))],
                  levels = LETTERS[1:4])

  result <- conformal_aps(x, y, model = oracle, x_new = x_new, alpha = 0.10,
                           seed = 1)

  # Deterministic scoring gave q = 1 and a mean set size of 3.90 out of 4 here.
  expect_lt(result$quantile, 1)
  expect_lt(mean(set_size(result)), 3)
  expect_gt(coverage(result, y_new), 0.85)
  expect_lt(coverage(result, y_new), 0.95)
})

test_that("APS on a binary problem does not always return both classes", {
  clf <- make_model(
    function(x, y) glm(y ~ ., data = data.frame(y = y, x), family = "binomial"),
    function(o, xn) {
      p <- predict(o, as.data.frame(xn), type = "response")
      cbind(A = 1 - p, B = p)
    },
    "classification"
  )

  set.seed(21)
  n <- 800
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- factor(ifelse(runif(n) < plogis(1.5 * x[, 1]), "B", "A"))
  x_new <- matrix(rnorm(300 * 2), ncol = 2)

  result <- conformal_aps(x, y, model = clf, x_new = x_new, alpha = 0.10,
                           seed = 1)

  expect_lt(mean(set_size(result)), 2)
  expect_true(any(set_size(result) == 1L))
})

test_that("randomized set construction is the exact inverse of the score", {
  probs <- matrix(c(0.6, 0.3, 0.1), nrow = 1)
  colnames(probs) <- c("A", "B", "C")

  # u = 0 recovers the deterministic rule: include class j iff cumsum_j <= q.
  set_at <- function(q) {
    predictset:::build_aps_sets(probs, q, randomize = FALSE,
                                allow_empty = TRUE)$sets[[1]]
  }
  expect_equal(set_at(0.5), character(0))
  expect_equal(set_at(0.6), "A")
  expect_equal(set_at(0.89), "A")
  expect_equal(set_at(0.9), c("A", "B"))
  expect_equal(set_at(1.0), c("A", "B", "C"))
})

test_that("allow_empty controls whether empty sets are returned", {
  probs <- matrix(c(0.6, 0.3, 0.1), nrow = 1)
  colnames(probs) <- c("A", "B", "C")

  empty <- predictset:::build_aps_sets(probs, 0.1, randomize = FALSE,
                                        allow_empty = TRUE)$sets[[1]]
  filled <- predictset:::build_aps_sets(probs, 0.1, randomize = FALSE,
                                         allow_empty = FALSE)$sets[[1]]

  expect_length(empty, 0L)
  expect_equal(filled, "A")
})

test_that("deterministic APS warns when the quantile saturates at 1", {
  # An uninformative model ranks the true class last more than alpha of the
  # time, so the deterministic score's atom at 1 becomes the quantile.
  noise <- make_model(
    function(x, y) NULL,
    function(o, xn) {
      m <- matrix(1 / 3, nrow = nrow(xn), ncol = 3)
      colnames(m) <- c("A", "B", "C")
      m
    },
    "classification"
  )
  set.seed(31)
  n <- 400
  x <- matrix(rnorm(n * 2), ncol = 2)
  y <- factor(sample(c("A", "B", "C"), n, replace = TRUE))

  expect_warning(
    conformal_aps(x, y, model = noise, x_new = x[1:10, ], randomize = FALSE,
                   seed = 1),
    "full label set"
  )
})

# --- Weighted conformal -------------------------------------------------------

test_that("weights_new gives a different quantile per test point", {
  set.seed(55)
  n <- 600
  x <- matrix(rnorm(n), ncol = 1)
  y <- x[, 1] + rnorm(n, sd = 0.2 + abs(x[, 1]))
  x_new <- matrix(c(rnorm(50, -2), rnorm(50, 3)), ncol = 1)
  w <- dnorm(x[, 1], 2) / dnorm(x[, 1], 0)
  w_new <- dnorm(x_new[, 1], 2) / dnorm(x_new[, 1], 0)

  result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
                                weights = w, weights_new = w_new, seed = 1)

  expect_gt(length(unique(round(result$quantile_by_point, 8))), 1L)
  expect_length(result$quantile_by_point, nrow(x_new))
})

test_that("omitting weights_new with varying weights warns", {
  set.seed(56)
  n <- 200
  x <- matrix(rnorm(n), ncol = 1)
  y <- x[, 1] + rnorm(n)
  w <- exp(x[, 1])

  expect_warning(
    conformal_weighted(x, y, model = y ~ ., x_new = x[1:10, , drop = FALSE],
                        weights = w, seed = 1),
    "weights_new"
  )
})

test_that("uniform weights do not warn and match split conformal", {
  set.seed(57)
  n <- 200
  x <- matrix(rnorm(n), ncol = 1)
  y <- x[, 1] + rnorm(n)
  x_new <- x[1:10, , drop = FALSE]

  expect_no_warning(
    w_result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
                                    weights = rep(1, n), seed = 1)
  )
  s_result <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 1)
  expect_equal(w_result$quantile, s_result$quantile)
})

# --- Mondrian -----------------------------------------------------------------

test_that("Mondrian per-group quantiles differ when group noise differs", {
  set.seed(66)
  n <- 1200
  x <- matrix(rnorm(n * 2), ncol = 2)
  groups <- factor(rep(c("tight", "wide"), length.out = n))
  y <- x[, 1] + ifelse(groups == "wide", 4, 0.25) * rnorm(n)
  x_new <- matrix(rnorm(200 * 2), ncol = 2)
  groups_new <- factor(rep(c("tight", "wide"), length.out = 200))

  result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
                                groups = groups, groups_new = groups_new,
                                seed = 1)

  expect_gt(result$group_quantiles[["wide"]],
            result$group_quantiles[["tight"]] * 3)
})

# --- RNG hygiene --------------------------------------------------------------

test_that("a seeded call leaves the global random stream untouched", {
  x <- matrix(rnorm(100), ncol = 2)
  y <- rnorm(50)
  x_new <- matrix(rnorm(10), ncol = 2)

  set.seed(123)
  expected <- rnorm(1)

  set.seed(123)
  invisible(conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 99))
  expect_equal(rnorm(1), expected)

  set.seed(123)
  invisible(conformal_cv(x, y, model = y ~ ., x_new = x_new, n_folds = 5,
                          seed = 99))
  expect_equal(rnorm(1), expected)

  set.seed(123)
  invisible(conformal_jackknife(x, y, model = y ~ ., x_new = x_new, seed = 99))
  expect_equal(rnorm(1), expected)
})

test_that("seeded calls remain reproducible", {
  x <- matrix(rnorm(200), ncol = 2)
  y <- rnorm(100)
  x_new <- matrix(rnorm(10), ncol = 2)

  a <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 7)
  b <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 7)
  expect_equal(a$quantile, b$quantile)
  expect_equal(a$lower, b$lower)
})

# --- Diagnostics and input handling -------------------------------------------

test_that("coverage_by_bin tolerates tied predictions", {
  set.seed(6)
  n <- 200
  x <- matrix(rep(c(0, 1), each = n / 2), ncol = 1)
  y <- x[, 1] + rnorm(n)
  x_new <- matrix(rep(c(0, 1), each = 50), ncol = 1)
  y_new <- x_new[, 1] + rnorm(100)

  result <- conformal_split(x, y, model = y ~ ., x_new = x_new, seed = 1)

  expect_warning(out <- coverage_by_bin(result, y_new, bins = 10),
                 "Tied predictions")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("non-numeric predictor columns are rejected clearly", {
  df <- data.frame(a = rnorm(100), grp = factor(sample(c("u", "v"), 100, TRUE)))
  y <- rnorm(100)

  expect_error(
    conformal_split(df, y, model = y ~ ., x_new = df[1:5, ]),
    "only numeric columns"
  )
})

test_that("x_new column count is checked for every method", {
  set.seed(8)
  x <- matrix(rnorm(200), ncol = 2)
  y <- rnorm(100)
  bad <- matrix(rnorm(9), ncol = 3)

  expect_error(conformal_split(x, y, y ~ ., bad), "must have 2 columns")
  expect_error(conformal_cv(x, y, y ~ ., bad, n_folds = 3), "must have 2 columns")
  expect_error(conformal_jackknife(x, y, y ~ ., bad), "must have 2 columns")
})

test_that("plot() survives unbounded intervals", {
  set.seed(77)
  x <- matrix(rnorm(200), ncol = 1)
  y <- x[, 1] + rnorm(200)
  x_new <- matrix(rnorm(5), ncol = 1)

  result <- conformal_split(x, y, model = y ~ ., x_new = x_new, alpha = 0.001,
                             seed = 1)
  expect_true(is.infinite(result$quantile))

  path <- tempfile(fileext = ".pdf")
  pdf(path)
  on.exit({
    dev.off()
    unlink(path)
  }, add = TRUE)
  expect_message(plot(result), "unbounded")
})

# --- CQR ----------------------------------------------------------------------

test_that("conformal_cqr records quantiles and flags a mismatch with alpha", {
  mlo <- make_model(function(x, y) lm(y ~ ., data.frame(y = y, x)),
                    function(o, xn) predict(o, as.data.frame(xn)) - 1.5,
                    "regression")
  mhi <- make_model(function(x, y) lm(y ~ ., data.frame(y = y, x)),
                    function(o, xn) predict(o, as.data.frame(xn)) + 1.5,
                    "regression")
  set.seed(4)
  x <- matrix(rnorm(400), ncol = 2)
  y <- x[, 1] + rnorm(200)
  x_new <- matrix(rnorm(10), ncol = 2)

  ok <- conformal_cqr(x, y, mlo, mhi, x_new = x_new, alpha = 0.10,
                       quantiles = c(0.05, 0.95), seed = 1)
  expect_equal(ok$quantiles, c(0.05, 0.95))

  expect_warning(
    conformal_cqr(x, y, mlo, mhi, x_new = x_new, alpha = 0.10,
                   quantiles = c(0.25, 0.75), seed = 1),
    "alpha"
  )
  expect_error(
    conformal_cqr(x, y, mlo, mhi, x_new = x_new, quantiles = c(0.95, 0.05)),
    "increasing"
  )
})

# --- Coverage is two-sided ----------------------------------------------------

test_that("split conformal coverage brackets the target, not just exceeds it", {
  set.seed(101)
  cov <- numeric(200)
  for (r in seq_len(200)) {
    x <- matrix(rnorm(400), ncol = 2)
    y <- x[, 1] * 2 + rnorm(200)
    x_new <- matrix(rnorm(100), ncol = 2)
    y_new <- x_new[, 1] * 2 + rnorm(50)
    result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
    cov[r] <- coverage(result, y_new)
  }
  # An over-covering method (the old APS failure mode) passes a one-sided
  # lower bound while being useless, so bound it on both sides.
  expect_gt(mean(cov), 0.87)
  expect_lt(mean(cov), 0.93)
})
