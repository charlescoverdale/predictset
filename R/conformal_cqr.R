#' Conformalized Quantile Regression
#'
#' Constructs prediction intervals using Conformalized Quantile Regression
#' (Romano et al. 2019). Requires two models: one for the lower quantile and
#' one for the upper quantile. The conformal step adjusts these quantile
#' predictions to achieve valid coverage.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model_lower A [make_model()] specification for the lower quantile
#'   model.
#' @param model_upper A [make_model()] specification for the upper quantile
#'   model.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param alpha Miscoverage level. Default `0.10`.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param quantiles The target quantiles. Default `c(0.05, 0.95)`.
#' @param seed Optional random seed.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"cqr"`.
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(20 * 3), ncol = 3)
#'
#' # Using simple linear models as stand-in for quantile regression
#' model_lo <- make_model(
#'   train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
#'   predict_fun = function(obj, x_new) {
#'     predict(obj, newdata = as.data.frame(x_new)) - 1.5
#'   },
#'   type = "regression"
#' )
#' model_hi <- make_model(
#'   train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
#'   predict_fun = function(obj, x_new) {
#'     predict(obj, newdata = as.data.frame(x_new)) + 1.5
#'   },
#'   type = "regression"
#' )
#'
#' result <- conformal_cqr(x, y, model_lo, model_hi, x_new = x_new)
#' print(result)
#'
#' @export
conformal_cqr <- function(x, y, model_lower, model_upper, x_new,
                            alpha = 0.10, cal_fraction = 0.5,
                            quantiles = c(0.05, 0.95), seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  x_new <- validate_x(x_new, "x_new")
  alpha <- validate_alpha(alpha)

  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod_lo <- resolve_model(model_lower, type = "regression")
  mod_hi <- resolve_model(model_upper, type = "regression")

  # Split data

  split <- split_data(nrow(x), cal_fraction, seed)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]

  # Train both quantile models
  fitted_lo <- mod_lo$train_fun(x_train, y_train)
  fitted_hi <- mod_hi$train_fun(x_train, y_train)

  # Calibration scores
  qhat_lo_cal <- mod_lo$predict_fun(fitted_lo, x_cal)
  qhat_hi_cal <- mod_hi$predict_fun(fitted_hi, x_cal)
  scores <- pmax(qhat_lo_cal - y_cal, y_cal - qhat_hi_cal)

  q <- conformal_quantile(scores, alpha)

  # Predictions on new data
  qhat_lo_new <- mod_lo$predict_fun(fitted_lo, x_new)
  qhat_hi_new <- mod_hi$predict_fun(fitted_hi, x_new)

  structure(list(
    pred = (qhat_lo_new + qhat_hi_new) / 2,
    lower = qhat_lo_new - q,
    upper = qhat_hi_new + q,
    alpha = alpha,
    method = "cqr",
    scores = scores,
    quantile = q,
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = list(lower = fitted_lo, upper = fitted_hi),
    model = list(lower = mod_lo, upper = mod_hi)
  ), class = "predictset_reg")
}
