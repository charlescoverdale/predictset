# Internal helper functions

# Minimum scale threshold for normalized conformal scoring
MIN_SCALE <- 1e-6

# Set the RNG seed for the duration of the calling function only, restoring the
# user's .Random.seed on exit. Packages should not leave the global random
# stream altered as a side effect of being called.
local_seed <- function(seed, frame = parent.frame()) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    assign(".predictset_old_seed",
           get(".Random.seed", envir = globalenv(), inherits = FALSE),
           envir = frame)
    restore <- quote(
      assign(".Random.seed", .predictset_old_seed, envir = globalenv())
    )
  } else {
    restore <- quote(
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    )
  }
  do.call(on.exit, list(restore, add = TRUE, after = FALSE), envir = frame)
  set.seed(seed)
  invisible(NULL)
}

validate_x <- function(x, arg = "x") {
  if (is.data.frame(x)) {
    bad <- names(x)[!vapply(x, is.numeric, logical(1))]
    if (length(bad) > 0) {
      cli_abort(c(
        "{.arg {arg}} must contain only numeric columns.",
        "x" = "Non-numeric column{?s}: {.val {bad}}.",
        "i" = "Encode factors as numeric columns first, for example with
               {.code stats::model.matrix(~ . - 1, data = {arg})}."
      ))
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x) && !is.numeric(x)) {
    cli_abort("{.arg {arg}} must be a numeric matrix or data frame.")
  }
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }
  if (!is.numeric(x)) {
    cli_abort(c(
      "{.arg {arg}} must be numeric, not {.cls {typeof(x)}}.",
      "i" = "Encode categorical predictors as numeric columns first."
    ))
  }
  if (nrow(x) == 0) {
    cli_abort("{.arg {arg}} must have at least one row.")
  }
  if (any(is.na(x))) {
    cli_abort("{.arg {arg}} must not contain NA values.")
  }
  if (any(!is.finite(x))) {
    cli_abort("{.arg {arg}} must not contain NaN or Inf values.")
  }
  # Ensure consistent column names so train/predict data frames match
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("X", seq_len(ncol(x)))
  }
  x
}

validate_y_reg <- function(y) {
  if (!is.numeric(y)) {
    cli_abort("{.arg y} must be a numeric vector for regression.")
  }

  if (any(is.na(y))) {
    cli_abort("{.arg y} must not contain NA values.")
  }
  if (any(!is.finite(y))) {
    cli_abort("{.arg y} must not contain NaN or Inf values.")
  }
  if (length(unique(y)) == 1) {
    cli_warn("{.arg y} has only one unique value. Prediction intervals will have zero width.")
  }
  y
}

validate_y_class <- function(y) {
  if (!is.factor(y)) {
    y <- factor(y)
  }
  if (any(is.na(y))) {
    cli_abort("{.arg y} must not contain NA values.")
  }
  y
}

validate_x_new <- function(x, x_new) {
  if (ncol(x) != ncol(x_new)) {
    cli_abort("{.arg x_new} must have {ncol(x)} column{?s}, not {ncol(x_new)}.")
  }
}

# Full validation of a predicted probability matrix: class columns, range, and
# row sums. Applied identically at calibration, prediction, and predict() time.
validate_probs <- function(probs, classes, arg = "probability matrix") {
  if (!is.matrix(probs) || !is.numeric(probs)) {
    cli_abort(c(
      "The {arg} must be a numeric matrix.",
      "i" = "{.arg predict_fun} must return one column per class, named by class label."
    ))
  }
  if (ncol(probs) < 2) {
    cli_abort("The {arg} must have at least 2 columns (classes).")
  }
  missing_levels <- setdiff(classes, colnames(probs))
  if (length(missing_levels) > 0) {
    cli_abort(
      "The {arg} is missing columns for class level{?s}: {.val {missing_levels}}."
    )
  }
  if (any(is.na(probs))) {
    cli_abort("The {arg} must not contain NA values.")
  }
  if (any(probs < 0 | probs > 1)) {
    cli_abort("The {arg} must contain probabilities in [0, 1].")
  }
  if (any(abs(rowSums(probs) - 1) > 0.01)) {
    cli_warn("Some rows of the {arg} do not sum to 1.")
  }
  invisible(probs)
}

# Attach class-label column names when predict_fun returned an unnamed matrix.
label_probs <- function(probs, classes) {
  if (is.null(dim(probs))) {
    probs <- as.matrix(probs)
  }
  if (is.null(colnames(probs))) {
    if (ncol(probs) != length(classes)) {
      cli_abort(c(
        "{.arg predict_fun} returned {ncol(probs)} unnamed column{?s} for {length(classes)} class{?es}.",
        "i" = "Name the columns of the probability matrix by class label."
      ))
    }
    colnames(probs) <- classes
  }
  probs
}

validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    cli_abort("{.arg alpha} must be a single number between 0 and 1 (exclusive).")
  }
  alpha
}

split_data <- function(n, cal_fraction) {
  n_cal <- floor(n * cal_fraction)
  n_train <- n - n_cal
  if (n_train < 2) {
    cli_abort("Not enough data for training after split. Reduce {.arg cal_fraction}.")
  }
  if (n_cal < 1) {
    cli_abort("Not enough data for calibration. Increase {.arg cal_fraction}.")
  }
  idx <- sample.int(n)
  list(
    train = idx[seq_len(n_train)],
    cal = idx[(n_train + 1):n]
  )
}

kfold_split <- function(n, n_folds) {
  idx <- sample.int(n)
  fold_ids <- rep(seq_len(n_folds), length.out = n)
  folds <- vector("list", n_folds)
  for (k in seq_len(n_folds)) {
    folds[[k]] <- idx[fold_ids == k]
  }
  folds
}

conformal_quantile <- function(scores, alpha) {
  n <- length(scores)
  k <- ceiling((n + 1) * (1 - alpha))
  if (k > n) return(Inf)
  sort(scores)[k]
}

# Smallest calibration size at which the conformal quantile is finite.
min_cal_size <- function(alpha) {
  ceiling(1 / alpha) - 1
}

# The k-th smallest of `values`, following the Barber et al. (2021) convention
# that an index outside 1..n denotes -Inf (side = "lower") or +Inf
# (side = "upper"). Clamping to the extreme order statistic instead would
# silently produce an interval narrower than the estimator is defined to be.
order_stat <- function(values_sorted, k, side = c("lower", "upper")) {
  side <- match.arg(side)
  n <- length(values_sorted)
  if (k < 1L) {
    return(if (side == "lower") -Inf else Inf)
  }
  if (k > n) {
    return(if (side == "upper") Inf else -Inf)
  }
  values_sorted[k]
}

regression_scores <- function(y, yhat) {
  abs(y - yhat)
}

# Classification helpers

# Generalized inverse quantile score of Romano, Sesia and Candes (2020):
#   E(x, y, u) = sum_{j: p_j >= p_y} p_j - u * p_y,  u ~ Unif(0, 1).
# The randomized form (u ~ U(0,1)) is the method as published; u = 0 is the
# deterministic simplification, which places an atom of mass at exactly 1 and
# is markedly conservative.
aps_scores <- function(probs, y_true, randomize = TRUE) {
  n <- nrow(probs)
  classes <- colnames(probs)
  scores <- numeric(n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    ord <- order(p, decreasing = TRUE)
    sorted_p <- p[ord]
    sorted_classes <- classes[ord]
    cumprobs <- cumsum(sorted_p)
    true_class <- as.character(y_true[i])
    rank_true <- which(sorted_classes == true_class)
    if (length(rank_true) == 0) {
      scores[i] <- 1.0
      next
    }
    score <- cumprobs[rank_true]
    if (randomize) {
      score <- score - stats::runif(1) * sorted_p[rank_true]
    }
    scores[i] <- score
  }
  scores
}

raps_scores <- function(probs, y_true, k_reg = 1, lambda = 0.01,
                         randomize = TRUE) {
  n <- nrow(probs)
  classes <- colnames(probs)
  scores <- numeric(n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    ord <- order(p, decreasing = TRUE)
    sorted_p <- p[ord]
    sorted_classes <- classes[ord]
    cumprobs <- cumsum(sorted_p)
    true_class <- as.character(y_true[i])
    rank_true <- which(sorted_classes == true_class)
    if (length(rank_true) == 0) {
      scores[i] <- 1.0 + lambda * max(0, length(classes) - k_reg)
      next
    }
    penalty <- lambda * max(0, rank_true - k_reg)
    score <- cumprobs[rank_true] + penalty
    if (randomize) {
      score <- score - stats::runif(1) * sorted_p[rank_true]
    }
    scores[i] <- score
  }
  scores
}

lac_scores <- function(probs, y_true) {
  y_idx <- match(as.character(y_true), colnames(probs))
  if (any(is.na(y_idx))) {
    unseen <- unique(as.character(y_true)[is.na(y_idx)])
    cli_abort(
      "True class label{?s} {.val {unseen}} not found in probability matrix columns."
    )
  }
  1 - probs[cbind(seq_len(nrow(probs)), y_idx)]
}

# Fall back to the most probable class when the score-inverted set is empty.
non_empty <- function(included, classes, p, allow_empty) {
  if (length(included) == 0 && !allow_empty) {
    return(classes[which.max(p)])
  }
  included
}

# Invert the APS score exactly: include class j iff its score is <= threshold.
# The per-observation u must be drawn once and shared across classes, exactly as
# in the score, so that the set is {y : E(x, y, u) <= q}. That quantity is
# non-decreasing in rank, so the set is always a prefix of the sorted classes.
build_aps_sets <- function(probs, threshold, randomize = TRUE,
                           allow_empty = FALSE) {
  n <- nrow(probs)
  classes <- colnames(probs)
  sets <- vector("list", n)
  set_probs <- vector("list", n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    ord <- order(p, decreasing = TRUE)
    sorted_p <- p[ord]
    sorted_classes <- classes[ord]
    cumprobs <- cumsum(sorted_p)
    u <- if (randomize) stats::runif(1) else 0
    crit <- cumprobs - u * sorted_p
    k <- sum(crit <= threshold)
    included <- if (k > 0) sorted_classes[seq_len(k)] else character(0)
    included <- non_empty(included, classes, p, allow_empty)
    sets[[i]] <- included
    set_probs[[i]] <- setNames(p[included], included)
  }
  list(sets = sets, probs = set_probs)
}

build_raps_sets <- function(probs, threshold, k_reg = 1, lambda = 0.01,
                            randomize = TRUE, allow_empty = FALSE) {
  n <- nrow(probs)
  classes <- colnames(probs)
  sets <- vector("list", n)
  set_probs <- vector("list", n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    ord <- order(p, decreasing = TRUE)
    sorted_p <- p[ord]
    sorted_classes <- classes[ord]
    cumprobs <- cumsum(sorted_p)
    penalties <- lambda * pmax(0, seq_along(sorted_p) - k_reg)
    u <- if (randomize) stats::runif(1) else 0
    crit <- cumprobs - u * sorted_p + penalties
    k <- sum(crit <= threshold)
    included <- if (k > 0) sorted_classes[seq_len(k)] else character(0)
    included <- non_empty(included, classes, p, allow_empty)
    sets[[i]] <- included
    set_probs[[i]] <- setNames(p[included], included)
  }
  list(sets = sets, probs = set_probs)
}

build_lac_sets <- function(probs, threshold, allow_empty = FALSE) {
  n <- nrow(probs)
  classes <- colnames(probs)
  sets <- vector("list", n)
  set_probs <- vector("list", n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    included <- classes[p >= 1 - threshold]
    included <- non_empty(included, classes, p, allow_empty)
    sets[[i]] <- included
    set_probs[[i]] <- setNames(p[included], included)
  }
  list(sets = sets, probs = set_probs)
}

# Warn when every prediction set is the full label set, which is a valid but
# useless answer. With deterministic APS/RAPS scoring this happens whenever the
# model ranks the true class last more often than alpha of the time: the score
# then has an atom at exactly 1 that becomes the conformal quantile.
warn_saturated <- function(sets, classes, method, randomize) {
  n_classes <- length(classes)
  if (length(sets) > 0 &&
      all(vapply(sets, length, integer(1)) == n_classes)) {
    msg <- c(
      "Every {method} prediction set is the full label set of {n_classes} class{?es}, which carries no information."
    )
    if (!randomize) {
      msg <- c(msg, "i" = "Deterministic scoring ({.code randomize = FALSE}) places an atom at 1 in the score. Use {.code randomize = TRUE} for the method as published.")
    } else {
      msg <- c(msg, "i" = "The model's class probabilities may carry little signal at this {.arg alpha}.")
    }
    cli_warn(msg)
  }
  invisible(NULL)
}

# Weighted conformal quantile per Tibshirani et al. (2019), Eq. 5:
#   q(x) = inf{q : sum_{i: s_i <= q} w_i / (sum_j w_j + w(x)) >= 1 - alpha}
# The test-point weight w(x) is known at test time and yields a different
# quantile for each test point; that per-point adaptation is the mechanism by
# which weighted conformal prediction corrects for covariate shift.
weighted_conformal_quantile <- function(scores, weights, alpha, w_test) {
  ord <- order(scores)
  sorted_scores <- scores[ord]
  cumw <- cumsum(weights[ord])
  total_weight <- sum(weights)

  vapply(w_test, function(wt) {
    idx <- which(cumw / (total_weight + wt) >= 1 - alpha)[1]
    if (is.na(idx)) Inf else sorted_scores[idx]
  }, numeric(1))
}
