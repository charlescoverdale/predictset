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
#' @param weights_new A numeric vector of importance weights for each
#'   observation in `x_new`, with length equal to `nrow(x_new)`. Supplying
#'   these gives the exact procedure of Tibshirani et al. (2019), in which
#'   each test point receives its own conformal quantile. If `NULL`, the mean
#'   calibration weight is substituted for every test point, which is an
#'   approximation (see Details).
#' @param alpha Miscoverage level. Default `0.10`.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param seed Optional random seed. Set for the duration of the call only;
#'   the global random stream is restored on exit.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"weighted"`. The `quantile` component is the
#'   median conformal quantile across test points; `quantile_by_point` holds
#'   the full vector.
#'
#' @details
#' Tibshirani et al. (2019), Equation 5, defines the weighted conformal
#' quantile using the test-point weight \eqn{w(X_{n+1})}, which is known at
#' test time. Each test point therefore receives a different quantile, and
#' that per-point adaptation is the mechanism by which the method corrects for
#' covariate shift. Supply `weights_new` to obtain it.
#'
#' A test point whose weight is large relative to the calibration weights
#' receives an infinite quantile: the point mass at \eqn{+\infty} carries more
#' than \eqn{\alpha} of the weighted distribution, so no finite interval is
#' justified there. That is the correct answer, and it flags test covariates
#' the calibration set cannot support.
#'
#' When `weights_new` is `NULL` the mean calibration weight is used for every
#' test point. This yields a single constant-width interval and does not carry
#' the finite-sample guarantee; it is offered only as a fallback for when the
#' likelihood ratio cannot be evaluated on the test covariates.
#'
#' @references
#' Tibshirani, R.J., Barber, R.F., Candes, E.J. and Ramdas, A. (2019).
#' Conformal prediction under covariate shift.
#' *Advances in Neural Information Processing Systems*, 32.
#'
#' Barber, R.F., Candes, E.J., Ramdas, A. and Tibshirani, R.J. (2023).
#' Conformal prediction beyond exchangeability.
#' *Annals of Statistics*, 51(2), 816-845.
#' \doi{10.1214/23-AOS2276}
#'
#' @examples
#' set.seed(42)
#' n <- 400
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(50 * 3, mean = 1), ncol = 3)
#'
#' # Likelihood ratio of the test density to the training density
#' w <- dnorm(x[, 1], mean = 1) / dnorm(x[, 1], mean = 0)
#' w_new <- dnorm(x_new[, 1], mean = 1) / dnorm(x_new[, 1], mean = 0)
#'
#' \donttest{
#' result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
#'                               weights = w, weights_new = w_new)
#' print(result)
#' }
#'
#' @family regression methods
#' @export
conformal_weighted <- function(x, y, model, x_new, weights = NULL,
                                weights_new = NULL,
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
  weights <- check_weights(weights, n, "weights")

  # With equal weights every test point has the same weight anyway, so the
  # mean is exact and there is nothing to warn about.
  uniform <- isTRUE(all.equal(weights, rep(weights[1], n)))

  if (is.null(weights_new)) {
    if (!uniform) {
      cli_warn(c(
        "{.arg weights_new} not supplied, so the mean calibration weight is used for every test point.",
        "i" = "This is an approximation. Supply {.arg weights_new} for the exact procedure of Tibshirani et al. (2019)."
      ))
    }
    w_test <- rep(mean(weights), nrow(x_new))
  } else {
    w_test <- check_weights(weights_new, nrow(x_new), "weights_new")
  }

  mod <- resolve_model(model, type = "regression")
  local_seed(seed)

  split <- split_data(n, cal_fraction)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]
  w_cal <- weights[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  yhat_cal <- mod$predict_fun(fitted, x_cal)
  scores <- regression_scores(y_cal, yhat_cal)

  q <- weighted_conformal_quantile(scores, w_cal, alpha, w_test)

  yhat_new <- mod$predict_fun(fitted, x_new)

  structure(list(
    pred = yhat_new,
    lower = yhat_new - q,
    upper = yhat_new + q,
    alpha = alpha,
    method = "weighted",
    scores = scores,
    quantile = stats::median(q),
    quantile_by_point = q,
    cal_weights = w_cal,
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod
  ), class = "predictset_reg")
}

check_weights <- function(w, n, arg) {
  if (!is.numeric(w)) {
    cli_abort("{.arg {arg}} must be a numeric vector.")
  }
  if (length(w) != n) {
    cli_abort("{.arg {arg}} must have length {n}, not {length(w)}.")
  }
  if (any(!is.finite(w))) {
    cli_abort("{.arg {arg}} must not contain NA, NaN, or Inf values.")
  }
  if (any(w < 0)) {
    cli_abort("{.arg {arg}} must be non-negative.")
  }
  if (sum(w) <= 0) {
    cli_abort("{.arg {arg}} must not be all zero.")
  }
  w
}
