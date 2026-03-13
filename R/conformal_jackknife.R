#' Jackknife+ Conformal Prediction Intervals
#'
#' Constructs prediction intervals using the Jackknife+ method of
#' Barber et al. (2021). Uses leave-one-out models to form prediction
#' intervals with finite-sample coverage guarantees.
#'
#' The Jackknife+ method fits n leave-one-out models and uses each model's
#' prediction at the test point, shifted by the corresponding LOO residual,
#' to construct the interval. This is distinct from basic jackknife, which
#' centres a single full-model prediction and adds a quantile of the LOO
#' residuals.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object (e.g., from [lm()]), a [make_model()]
#'   specification, or a formula (which will fit a linear model).
#' @param x_new A numeric matrix or data frame of new predictor variables.
#'   If `NULL`, intervals are computed for the training data.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction
#'   intervals.
#' @param plus Logical. If `TRUE` (default), uses the Jackknife+ method.
#'   If `FALSE`, uses the basic jackknife.
#' @param verbose Logical. If `TRUE`, shows a progress bar during LOO fitting.
#'   Default `FALSE`.
#' @param seed Optional random seed for reproducible data splitting.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"jackknife_plus"` or `"jackknife"`.
#'   Additional components include `loo_models` (list of n leave-one-out
#'   fitted models) and `loo_residuals` (numeric vector of LOO absolute
#'   residuals).
#'
#' @references
#' Barber, R.F., Candes, E.J., Ramdas, A. and Tibshirani, R.J. (2021).
#' Predictive inference with the jackknife+.
#' *Annals of Statistics*, 49(1), 486--507.
#'
#' @examples
#' set.seed(42)
#' n <- 50
#' x <- matrix(rnorm(n * 2), ncol = 2)
#' y <- x[, 1] + rnorm(n)
#' x_new <- matrix(rnorm(10 * 2), ncol = 2)
#'
#' \donttest{
#' result <- conformal_jackknife(x, y, model = y ~ ., x_new = x_new)
#' print(result)
#' }
#'
#' @export
conformal_jackknife <- function(x, y, model, x_new = NULL, alpha = 0.10,
                                 plus = TRUE, verbose = FALSE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  alpha <- validate_alpha(alpha)
  n <- nrow(x)

  if (n != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "regression")
  if (!is.null(seed)) set.seed(seed)

  if (n > 500) {
    cli_inform("Fitting {n} leave-one-out models...")
  }

  # Leave-one-out: fit n models, compute LOO predictions at training points
  loo_models <- vector("list", n)
  loo_preds <- numeric(n)

  if (verbose) {
    cli_progress_bar("Fitting LOO models", total = n)
  }
  for (i in seq_len(n)) {
    train_idx <- setdiff(seq_len(n), i)
    loo_models[[i]] <- mod$train_fun(x[train_idx, , drop = FALSE], y[train_idx])
    loo_preds[i] <- mod$predict_fun(loo_models[[i]], x[i, , drop = FALSE])
    if (verbose) cli_progress_update()
  }
  if (verbose) cli_progress_done()

  loo_residuals <- abs(y - loo_preds)

  # Fit full model on all data for point prediction
  fitted_all <- mod$train_fun(x, y)

  # Determine test points
  if (!is.null(x_new)) {
    x_new <- validate_x(x_new, "x_new")
    x_test <- x_new
  } else {
    x_test <- x
  }

  # Full-model point predictions
  yhat_all <- mod$predict_fun(fitted_all, x_test)
  n_test <- nrow(x_test)

  if (plus) {
    # Jackknife+ (Barber et al. 2021):
    # For each test point j, compute yhat_{-i}(x_test_j) from each LOO model,
    # then lower = quantile of {yhat_{-i} - R_i}, upper = quantile of {yhat_{-i} + R_i}
    lower <- numeric(n_test)
    upper <- numeric(n_test)

    k_lo <- floor(alpha * (n + 1))
    k_hi <- ceiling((1 - alpha) * (n + 1))

    for (j in seq_len(n_test)) {
      loo_preds_at_j <- numeric(n)
      for (i in seq_len(n)) {
        loo_preds_at_j[i] <- mod$predict_fun(
          loo_models[[i]], x_test[j, , drop = FALSE]
        )
      }
      lower_vals <- sort(loo_preds_at_j - loo_residuals)
      upper_vals <- sort(loo_preds_at_j + loo_residuals)

      lower[j] <- lower_vals[max(k_lo, 1)]
      upper[j] <- upper_vals[min(k_hi, n)]
    }
    method <- "jackknife_plus"
  } else {
    # Basic jackknife: centre on full-model prediction +/- quantile of LOO residuals
    q <- conformal_quantile(loo_residuals, alpha)
    lower <- yhat_all - q
    upper <- yhat_all + q
    method <- "jackknife"
  }

  structure(list(
    pred = yhat_all,
    lower = lower,
    upper = upper,
    alpha = alpha,
    method = method,
    scores = loo_residuals,
    quantile = if (!plus) q else NA_real_,
    n_cal = n,
    n_train = n,
    fitted_model = fitted_all,
    model = mod,
    loo_models = loo_models,
    loo_residuals = loo_residuals
  ), class = "predictset_reg")
}
