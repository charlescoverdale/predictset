#' CV+ Conformal Prediction Intervals
#'
#' Constructs prediction intervals using the CV+ method of Barber et al. (2021).
#' Cross-validation residuals and fold-specific models are used to form
#' observation-specific prediction intervals with finite-sample coverage
#' guarantees.
#'
#' Unlike basic CV conformal prediction (which computes a single quantile of
#' CV residuals), CV+ constructs intervals that vary per test point. For each
#' test point, every training observation contributes a lower and upper value
#' based on the fold model that excluded that observation, evaluated at the
#' test point, plus or minus the leave-fold-out residual for that observation.
#' The interval bounds are then taken as quantiles of these per-observation
#' values.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object (e.g., from [lm()]), a [make_model()]
#'   specification, or a formula (which will fit a linear model).
#' @param x_new A numeric matrix or data frame of new predictor variables.
#'   If `NULL`, intervals are computed for the training data using
#'   leave-one-fold-out predictions. Note: when `x_new = NULL`, prediction
#'   intervals for training observations use a self-consistent approximation.
#'   For exact CV+ intervals on new data, provide `x_new`.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction
#'   intervals.
#' @param n_folds Number of cross-validation folds. Default `10`.
#' @param verbose Logical. If `TRUE`, shows a progress bar during fold fitting.
#'   Default `FALSE`.
#' @param seed Optional random seed for reproducible data splitting.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"cv_plus"`. The object also stores
#'   `fold_models` (list of K fitted models), `fold_ids` (integer vector
#'   mapping each observation to its fold), and `residuals` (leave-fold-out
#'   absolute residuals), which are needed by the [predict()] method to
#'   compute CV+ intervals for new data.
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(20 * 3), ncol = 3)
#'
#' \donttest{
#' result <- conformal_cv(x, y, model = y ~ ., x_new = x_new, n_folds = 5)
#' print(result)
#' }
#'
#' @export
conformal_cv <- function(x, y, model, x_new = NULL, alpha = 0.10,
                          n_folds = 10, verbose = FALSE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  alpha <- validate_alpha(alpha)
  n <- nrow(x)

  if (n != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }
  if (n_folds < 2 || n_folds > n) {
    cli_abort("{.arg n_folds} must be between 2 and {n}.")
  }

  mod <- resolve_model(model, type = "regression")
  folds <- kfold_split(n, n_folds, seed)

  # Build fold_ids: integer vector mapping observation index -> fold number
  fold_ids <- integer(n)
  for (k in seq_len(n_folds)) {
    fold_ids[folds[[k]]] <- k
  }

  # Fit K fold models, compute LOO predictions and residuals
  fold_models <- vector("list", n_folds)
  loo_preds <- numeric(n)

  if (verbose) {
    cli_progress_bar("Fitting fold models", total = n_folds)
  }
  for (k in seq_len(n_folds)) {
    test_idx <- folds[[k]]
    train_idx <- setdiff(seq_len(n), test_idx)
    fold_models[[k]] <- mod$train_fun(x[train_idx, , drop = FALSE], y[train_idx])
    loo_preds[test_idx] <- mod$predict_fun(fold_models[[k]],
                                            x[test_idx, , drop = FALSE])
    if (verbose) cli_progress_update()
  }
  if (verbose) cli_progress_done()

  residuals <- abs(y - loo_preds)

  # Fit full model on all data (for point predictions)
  fitted_all <- mod$train_fun(x, y)

  # Compute CV+ intervals
  if (!is.null(x_new)) {
    x_new <- validate_x(x_new, "x_new")
    yhat_full <- mod$predict_fun(fitted_all, x_new)
    intervals <- cv_plus_intervals(x_new, mod, fold_models, fold_ids,
                                    residuals, alpha, n)
  } else {
    yhat_full <- mod$predict_fun(fitted_all, x)
    # For training data: observation i in fold k uses loo_preds[i] as its
    # fold prediction, so we can compute intervals directly
    intervals <- cv_plus_intervals_train(loo_preds, fold_ids, residuals,
                                          alpha, n)
  }

  # Summary quantile for display (median half-width)
  half_widths <- (intervals$upper - intervals$lower) / 2
  display_quantile <- stats::median(half_widths)

  structure(list(
    pred = yhat_full,
    lower = intervals$lower,
    upper = intervals$upper,
    alpha = alpha,
    method = "cv_plus",
    scores = residuals,
    quantile = display_quantile,
    n_cal = n,
    n_train = n,
    fitted_model = fitted_all,
    model = mod,
    fold_models = fold_models,
    fold_ids = fold_ids,
    residuals = residuals
  ), class = "predictset_reg")
}


#' Compute CV+ intervals for new test points
#'
#' @param x_new Matrix of new test points.
#' @param mod Model specification with predict_fun.
#' @param fold_models List of K fitted fold models.
#' @param fold_ids Integer vector mapping each training obs to its fold.
#' @param residuals Numeric vector of leave-fold-out residuals.
#' @param alpha Miscoverage level.
#' @param n Number of training observations.
#'
#' @return List with `lower` and `upper` numeric vectors.
#'
#' @noRd
cv_plus_intervals <- function(x_new, mod, fold_models, fold_ids, residuals,
                               alpha, n) {
  n_folds <- length(fold_models)
  n_new <- nrow(x_new)

  # Get predictions from each fold model at all test points
  # fold_preds_matrix[j, k] = yhat_{-k}(x_new_j)
  fold_preds_matrix <- matrix(NA_real_, nrow = n_new, ncol = n_folds)
  for (k in seq_len(n_folds)) {
    fold_preds_matrix[, k] <- mod$predict_fun(fold_models[[k]], x_new)
  }

  lower <- numeric(n_new)
  upper <- numeric(n_new)

  k_lo <- floor(alpha * (n + 1))
  k_hi <- ceiling((1 - alpha) * (n + 1))

  for (j in seq_len(n_new)) {
    # For each training observation i in fold k(i):
    #   lower_value_i = yhat_{-k(i)}(x_new_j) - R_i
    #   upper_value_i = yhat_{-k(i)}(x_new_j) + R_i
    fold_pred_per_obs <- fold_preds_matrix[j, fold_ids]
    lower_values <- fold_pred_per_obs - residuals
    upper_values <- fold_pred_per_obs + residuals

    lower[j] <- sort(lower_values)[max(k_lo, 1L)]
    upper[j] <- sort(upper_values)[min(k_hi, n)]
  }

  list(lower = lower, upper = upper)
}


#' Compute CV+ intervals for training data
#'
#' For training observation i in fold k(i), the fold prediction at x_i is
#' already stored as loo_preds[i]. For the interval contribution from
#' observation j (in fold k(j)), we need yhat_{-k(j)}(x_i). Since we are
#' computing intervals at x_i (a training point), the fold prediction for
#' observation j at point x_i comes from model k(j).
#'
#' However, recomputing all n*K predictions would be expensive. Instead, for
#' training data, we use a simplification: observation i gets its interval
#' from the same quantile construction, but using loo_preds to define the
#' fold predictions.
#'
#' @param loo_preds Numeric vector of leave-fold-out predictions for training
#'   data.
#' @param fold_ids Integer vector mapping each training obs to its fold.
#' @param residuals Numeric vector of leave-fold-out residuals.
#' @param alpha Miscoverage level.
#' @param n Number of training observations.
#'
#' @return List with `lower` and `upper` numeric vectors.
#'
#' @noRd
cv_plus_intervals_train <- function(loo_preds, fold_ids, residuals, alpha, n) {
  # For training data, each observation i's interval is based on:
  # - Its own LOO prediction from its fold model: loo_preds[i]
  # - But the CV+ formula still uses ALL training observations
  #   For observation j's contribution at point x_i, we would need

  #   yhat_{-k(j)}(x_i), which is only = loo_preds[i] when k(j) = k(i).
  #
  # The exact approach would require n * n_folds predictions. For training
  # data, we use a self-consistent approximation: the interval for
  # observation i uses loo_preds[i] as the center (since that's the
  # leave-fold-out prediction for that point), and applies the standard
  # CV+ quantile logic with all residuals.
  #
  # This is equivalent to: for each training point i,
  # lower_value_j = loo_preds[i] - R_j for all j
  # upper_value_j = loo_preds[i] + R_j for all j
  # Then take quantiles.

  k_lo <- floor(alpha * (n + 1))
  k_hi <- ceiling((1 - alpha) * (n + 1))

  sorted_resid <- sort(residuals)
  # lower = loo_preds[i] - sorted_resid[n - k_lo + 1] (since subtracting
  # residual and taking low quantile = center - high quantile of residuals)
  # upper = loo_preds[i] + sorted_resid[min(k_hi, n)]

  q_upper <- sorted_resid[min(k_hi, n)]
  # For lower: the k_lo-th smallest of (center - R_j) = center - (k_lo-th
  # largest of R_j) = center - sorted_resid[n - max(k_lo, 1L) + 1]
  q_lower <- sorted_resid[n - max(k_lo, 1L) + 1L]

  lower <- loo_preds - q_lower
  upper <- loo_preds + q_upper

  list(lower = lower, upper = upper)
}
