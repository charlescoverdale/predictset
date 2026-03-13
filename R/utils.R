# Internal helper functions

validate_x <- function(x, arg = "x") {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x) && !is.numeric(x)) {
    cli_abort("{.arg {arg}} must be a numeric matrix or data frame.")
  }
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }
  if (nrow(x) == 0) {
    cli_abort("{.arg {arg}} must have at least one row.")
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
  y
}

validate_y_class <- function(y) {

  if (!is.factor(y)) {
    y <- factor(y)
  }
  y
}

validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    cli_abort("{.arg alpha} must be a single number between 0 and 1 (exclusive).")
  }
  alpha
}

split_data <- function(n, cal_fraction, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
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

kfold_split <- function(n, n_folds, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
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

regression_scores <- function(y, yhat) {
  abs(y - yhat)
}

# Classification helpers

aps_scores <- function(probs, y_true, randomize = FALSE) {
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
    score <- cumprobs[rank_true]
    if (randomize && rank_true >= 1) {
      u <- stats::runif(1)
      score <- score - u * sorted_p[rank_true]
    }
    scores[i] <- score
  }
  scores
}

raps_scores <- function(probs, y_true, k_reg = 1, lambda = 0.01,
                         randomize = FALSE) {
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
    penalty <- lambda * max(0, rank_true - k_reg)
    score <- cumprobs[rank_true] + penalty
    if (randomize && rank_true >= 1) {
      u <- stats::runif(1)
      score <- score - u * sorted_p[rank_true]
    }
    scores[i] <- score
  }
  scores
}

lac_scores <- function(probs, y_true) {
  y_idx <- match(as.character(y_true), colnames(probs))
  1 - probs[cbind(seq_len(nrow(probs)), y_idx)]
}

build_aps_sets <- function(probs, threshold) {
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
    k <- which(cumprobs >= threshold)[1]
    if (is.na(k)) k <- length(classes)
    included <- sorted_classes[seq_len(k)]
    sets[[i]] <- included
    set_probs[[i]] <- setNames(sorted_p[seq_len(k)], included)
  }
  list(sets = sets, probs = set_probs)
}

build_raps_sets <- function(probs, threshold, k_reg = 1, lambda = 0.01) {
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
    penalized <- cumprobs + penalties
    k <- which(penalized >= threshold)[1]
    if (is.na(k)) k <- length(classes)
    included <- sorted_classes[seq_len(k)]
    sets[[i]] <- included
    set_probs[[i]] <- setNames(sorted_p[seq_len(k)], included)
  }
  list(sets = sets, probs = set_probs)
}

build_lac_sets <- function(probs, threshold) {
  n <- nrow(probs)
  classes <- colnames(probs)
  sets <- vector("list", n)
  set_probs <- vector("list", n)

  for (i in seq_len(n)) {
    p <- probs[i, ]
    included <- classes[p >= 1 - threshold]
    if (length(included) == 0) {
      included <- classes[which.max(p)]
    }
    sets[[i]] <- included
    set_probs[[i]] <- setNames(p[included], included)
  }
  list(sets = sets, probs = set_probs)
}
