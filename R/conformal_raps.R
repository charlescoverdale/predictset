#' Regularized Adaptive Prediction Sets
#'
#' Constructs prediction sets using the Regularized Adaptive Prediction Sets
#' (RAPS) method of Angelopoulos et al. (2021). Extends APS with a
#' regularization penalty that encourages smaller prediction sets.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A factor (or character/integer vector coerced to factor) of class
#'   labels.
#' @param model A [make_model()] specification with `type = "classification"`,
#'   or a fitted model object that produces class probabilities.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction sets.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param seed Optional random seed.
#' @param k_reg Regularization parameter controlling the number of classes
#'   exempt from the penalty. Default `1` (only the top class is unpenalized).
#' @param lambda Regularization strength. Default `0.01`. Larger values
#'   produce smaller prediction sets at the potential cost of coverage.
#' @param randomize Logical. If `TRUE`, uses randomized scores for exact
#'   coverage (but prediction sets become stochastic). Default `FALSE`.
#'
#' @return A `predictset_class` object. See [conformal_class_split()] for
#'   details. The `method` component is `"raps"`.
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' y <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
#' x_new <- matrix(rnorm(50 * 4), ncol = 4)
#'
#' clf <- make_model(
#'   train_fun = function(x, y) glm(y ~ ., data = data.frame(y = y, x),
#'                                   family = "binomial"),
#'   predict_fun = function(object, x_new) {
#'     df <- as.data.frame(x_new)
#'     names(df) <- paste0("X", seq_len(ncol(x_new)))
#'     p <- predict(object, newdata = df, type = "response")
#'     cbind(A = p / 2, B = p / 2, C = 1 - p)
#'   },
#'   type = "classification"
#' )
#'
#' \donttest{
#' result <- conformal_raps(x, y, model = clf, x_new = x_new,
#'                           k_reg = 1, lambda = 0.01)
#' print(result)
#' }
#'
#' @export
conformal_raps <- function(x, y, model, x_new, alpha = 0.10,
                            cal_fraction = 0.5, k_reg = 1, lambda = 0.01,
                            randomize = FALSE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_class(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "classification")

  split <- split_data(nrow(x), cal_fraction, seed)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  probs_cal <- mod$predict_fun(fitted, x_cal)
  if (is.null(colnames(probs_cal))) {
    colnames(probs_cal) <- levels(y)
  }
  validate_probs_colnames(probs_cal, y, "calibration probability matrix")

  scores <- raps_scores(probs_cal, y_cal, k_reg = k_reg, lambda = lambda,
                         randomize = randomize)
  q <- conformal_quantile(scores, alpha)

  probs_new <- mod$predict_fun(fitted, x_new)
  if (is.null(colnames(probs_new))) {
    colnames(probs_new) <- levels(y)
  }

  result <- build_raps_sets(probs_new, q, k_reg = k_reg, lambda = lambda)

  structure(list(
    sets = result$sets,
    probs = result$probs,
    alpha = alpha,
    method = "raps",
    scores = scores,
    quantile = q,
    classes = levels(y),
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod,
    k_reg = k_reg,
    lambda = lambda,
    randomize = randomize
  ), class = "predictset_class")
}
