#' Weighted Conformal Prediction Intervals
#'
#' Constructs prediction intervals using weighted split conformal inference,
#' designed for settings with covariate shift where calibration and test data
#' may have different distributions. Importance weights re-weight the
#' calibration scores to account for this shift.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object, a [make_model()] specification, or a
#'   formula.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param weights A numeric vector of importance weights for each observation
#'   in `x`, with length equal to `nrow(x)`. Weights must be non-negative.
#'   If `NULL`, uniform weights are used (equivalent to standard split
#'   conformal).
#' @param alpha Miscoverage level. Default `0.10`.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param seed Optional random seed.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"weighted"`.
#'
#' @references
#' Tibshirani, R.J., Barber, R.F., Candes, E.J. and Ramdas, A. (2019).
#' Conformal prediction under covariate shift.
#' *Advances in Neural Information Processing Systems*, 32.
#'
#' @examples
#' set.seed(42)
#' n <- 400
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(50 * 3, mean = 1), ncol = 3)
#' weights <- rep(1, n)
#'
#' \donttest{
#' result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
#'                               weights = weights)
#' print(result)
#' }
#'
#' @export
conformal_weighted <- function(x, y, model, x_new, weights = NULL,
                                alpha = 0.10, cal_fraction = 0.5,
                                seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  n <- nrow(x)
  if (n != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  if (is.null(weights)) {
    weights <- rep(1, n)
  }

  if (length(weights) != n) {
    cli_abort("{.arg weights} must have length equal to {.code nrow(x)} ({n}).")
  }
  if (any(weights < 0)) {
    cli_abort("{.arg weights} must be non-negative.")
  }
  if (any(!is.finite(weights))) {
    cli_abort("{.arg weights} must not contain NA, NaN, or Inf values.")
  }

  mod <- resolve_model(model, type = "regression")

  split <- split_data(n, cal_fraction, seed)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]
  w_cal <- weights[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  yhat_cal <- mod$predict_fun(fitted, x_cal)
  scores <- regression_scores(y_cal, yhat_cal)

  q <- weighted_conformal_quantile(scores, w_cal, alpha)

  yhat_new <- mod$predict_fun(fitted, x_new)

  structure(list(
    pred = yhat_new,
    lower = yhat_new - q,
    upper = yhat_new + q,
    alpha = alpha,
    method = "weighted",
    scores = scores,
    quantile = q,
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod
  ), class = "predictset_reg")
}
