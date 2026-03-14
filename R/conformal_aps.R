#' Adaptive Prediction Sets
#'
#' Constructs prediction sets using the Adaptive Prediction Sets (APS) method
#' of Romano, Sesia, and Candes (2020). Classes are included in order of
#' decreasing predicted probability until the cumulative probability exceeds
#' the conformal threshold.
#'
#' @details
#' When \code{randomize = FALSE} (the default), this implementation uses the
#' deterministic variant of APS, which provides conservative coverage (at
#' least \eqn{1 - \alpha}). The randomized variant (\code{randomize = TRUE})
#' achieves exact \eqn{1 - \alpha} coverage but produces non-reproducible
#' prediction sets.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A factor (or character/integer vector coerced to factor) of class
#'   labels.
#' @param model A [make_model()] specification with `type = "classification"`,
#'   or a fitted model object that produces class probabilities.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction sets.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param randomize Logical. If `TRUE`, uses randomized scores for exact
#'   coverage (but prediction sets become stochastic). Default `FALSE`.
#' @param seed Optional random seed.
#'
#' @return A `predictset_class` object. See [conformal_lac()] for
#'   details. The `method` component is `"aps"`.
#'
#' @references
#' Romano, Y., Sesia, M. and Candes, E.J. (2020).
#' Classification with valid and adaptive coverage.
#' *Advances in Neural Information Processing Systems*, 33.
#' \doi{10.48550/arXiv.2006.02544}
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
#' result <- conformal_aps(x, y, model = clf, x_new = x_new)
#' print(result)
#' }
#'
#' @family classification methods
#' @export
conformal_aps <- function(x, y, model, x_new, alpha = 0.10,
                           cal_fraction = 0.5, randomize = FALSE,
                           seed = NULL) {
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

  scores <- aps_scores(probs_cal, y_cal, randomize = randomize)
  q <- conformal_quantile(scores, alpha)

  probs_new <- mod$predict_fun(fitted, x_new)
  if (is.null(colnames(probs_new))) {
    colnames(probs_new) <- levels(y)
  }

  result <- build_aps_sets(probs_new, q)

  structure(list(
    sets = result$sets,
    probs = result$probs,
    alpha = alpha,
    method = "aps",
    scores = scores,
    quantile = q,
    classes = levels(y),
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod,
    randomize = randomize
  ), class = "predictset_class")
}
