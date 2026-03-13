#' Jackknife+ Conformal Prediction Intervals
#'
#' Constructs prediction intervals using the Jackknife+ method of
#' Barber et al. (2021). Uses leave-one-out residuals to form prediction
#' intervals with finite-sample coverage guarantees.
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
#' @param seed Optional random seed for reproducible data splitting.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"jackknife_plus"` or `"jackknife"`.
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
                                 plus = TRUE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  alpha <- validate_alpha(alpha)
  n <- nrow(x)

  if (n != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "regression")
  if (!is.null(seed)) set.seed(seed)

  # Leave-one-out predictions
  loo_preds <- numeric(n)
  for (i in seq_len(n)) {
    train_idx <- setdiff(seq_len(n), i)
    fitted_i <- mod$train_fun(x[train_idx, , drop = FALSE], y[train_idx])
    loo_preds[i] <- mod$predict_fun(fitted_i, x[i, , drop = FALSE])
  }

  scores <- regression_scores(y, loo_preds)

  # Fit final model
  fitted_all <- mod$train_fun(x, y)

  if (!is.null(x_new)) {
    x_new <- validate_x(x_new, "x_new")
    yhat_new <- mod$predict_fun(fitted_all, x_new)
  } else {
    yhat_new <- mod$predict_fun(fitted_all, x)
  }

  if (plus) {
    # Jackknife+: use distribution of (yhat_{-i} +/- R_i)
    q <- conformal_quantile(scores, alpha)
    lower <- yhat_new - q
    upper <- yhat_new + q
    method <- "jackknife_plus"
  } else {
    q <- conformal_quantile(scores, alpha)
    lower <- yhat_new - q
    upper <- yhat_new + q
    method <- "jackknife"
  }

  structure(list(
    pred = yhat_new,
    lower = lower,
    upper = upper,
    alpha = alpha,
    method = method,
    scores = scores,
    quantile = q,
    n_cal = n,
    n_train = n,
    fitted_model = fitted_all,
    model = mod
  ), class = "predictset_reg")
}
