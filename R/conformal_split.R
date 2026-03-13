#' Split Conformal Prediction Intervals
#'
#' Constructs prediction intervals using split conformal inference. The data
#' is split into training and calibration sets; nonconformity scores are
#' computed on the calibration set and used to form intervals on new data.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object (e.g., from [lm()]), a [make_model()]
#'   specification, or a formula (which will fit a linear model).
#' @param x_new A numeric matrix or data frame of new predictor variables for
#'   which to compute prediction intervals.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction
#'   intervals.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param seed Optional random seed for reproducible data splitting.
#'
#' @return A `predictset_reg` object (a list) with components:
#' \describe{
#'   \item{pred}{Numeric vector of point predictions for `x_new`.}
#'   \item{lower}{Numeric vector of lower bounds.}
#'   \item{upper}{Numeric vector of upper bounds.}
#'   \item{alpha}{The miscoverage level used.}
#'   \item{method}{Character string `"split"`.}
#'   \item{scores}{Numeric vector of calibration nonconformity scores.}
#'   \item{quantile}{The conformal quantile used to form intervals.}
#'   \item{n_cal}{Number of calibration observations.}
#'   \item{n_train}{Number of training observations.}
#'   \item{fitted_model}{The fitted model object.}
#'   \item{model}{The `predictset_model` specification.}
#' }
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(50 * 3), ncol = 3)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' print(result)
#'
#' @export
conformal_split <- function(x, y, model, x_new, alpha = 0.10,
                             cal_fraction = 0.5, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  x_new <- validate_x(x_new, "x_new")
  alpha <- validate_alpha(alpha)

  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "regression")

  # Split data
  split <- split_data(nrow(x), cal_fraction, seed)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]

  # Train
  fitted <- mod$train_fun(x_train, y_train)

  # Calibration scores
  yhat_cal <- mod$predict_fun(fitted, x_cal)
  scores <- regression_scores(y_cal, yhat_cal)

  # Conformal quantile
  q <- conformal_quantile(scores, alpha)

  # Predictions on new data
  yhat_new <- mod$predict_fun(fitted, x_new)

  structure(list(
    pred = yhat_new,
    lower = yhat_new - q,
    upper = yhat_new + q,
    alpha = alpha,
    method = "split",
    scores = scores,
    quantile = q,
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod
  ), class = "predictset_reg")
}
