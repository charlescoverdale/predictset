#' CV+ Conformal Prediction Intervals
#'
#' Constructs prediction intervals using the CV+ method of Barber et al. (2021).
#' Cross-validation residuals are used to form prediction intervals with
#' finite-sample coverage guarantees.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object (e.g., from [lm()]), a [make_model()]
#'   specification, or a formula (which will fit a linear model).
#' @param x_new A numeric matrix or data frame of new predictor variables.
#'   If `NULL`, intervals are computed for the training data using
#'   leave-one-fold-out predictions.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction
#'   intervals.
#' @param n_folds Number of cross-validation folds. Default `10`.
#' @param seed Optional random seed for reproducible data splitting.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"cv_plus"`.
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
                          n_folds = 10, seed = NULL) {
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

  # LOO-fold predictions and residuals
  loo_preds <- numeric(n)

  for (k in seq_len(n_folds)) {
    test_idx <- folds[[k]]
    train_idx <- setdiff(seq_len(n), test_idx)
    fitted_k <- mod$train_fun(x[train_idx, , drop = FALSE], y[train_idx])
    loo_preds[test_idx] <- mod$predict_fun(fitted_k, x[test_idx, , drop = FALSE])
  }

  scores <- regression_scores(y, loo_preds)
  q <- conformal_quantile(scores, alpha)

  # Fit final model on all data
  fitted_all <- mod$train_fun(x, y)

  if (!is.null(x_new)) {
    x_new <- validate_x(x_new, "x_new")
    yhat_new <- mod$predict_fun(fitted_all, x_new)
  } else {
    yhat_new <- mod$predict_fun(fitted_all, x)
  }

  structure(list(
    pred = yhat_new,
    lower = yhat_new - q,
    upper = yhat_new + q,
    alpha = alpha,
    method = "cv_plus",
    scores = scores,
    quantile = q,
    n_cal = n,
    n_train = n,
    fitted_model = fitted_all,
    model = mod
  ), class = "predictset_reg")
}
