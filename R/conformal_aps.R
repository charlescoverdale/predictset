#' Adaptive Prediction Sets
#'
#' Constructs prediction sets using the Adaptive Prediction Sets (APS) method
#' of Romano, Sesia, and Candes (2020). Classes are included in order of
#' decreasing predicted probability until the cumulative probability exceeds
#' the conformal threshold.
#'
#' @details
#' \code{randomize = TRUE} (the default) is the method as published: the
#' nonconformity score carries a uniform random variable \eqn{U}, drawn once
#' per observation and shared across classes, and the prediction set is the
#' exact inversion of that score. This is what delivers coverage close to
#' \eqn{1 - \alpha} rather than well above it.
#'
#' \code{randomize = FALSE} uses the deterministic simplification \eqn{U = 0}.
#' It is reproducible without a seed, but the score then has an atom of
#' probability mass at exactly 1, hit whenever the model ranks the true class
#' last. Whenever that happens more often than \eqn{\alpha} of the time the
#' conformal quantile is exactly 1 and every prediction set is the full label
#' set; the function warns when this occurs. Deterministic scoring is
#' materially conservative for small numbers of classes.
#'
#' Randomized sets are stochastic: pass `seed`, or call [set.seed()], for
#' reproducible output.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A factor (or character/integer vector coerced to factor) of class
#'   labels.
#' @param model A [make_model()] specification with `type = "classification"`,
#'   or a fitted model object that produces class probabilities.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction sets.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param randomize Logical. If `TRUE` (the default), uses the randomized
#'   score and set construction of Romano, Sesia and Candes (2020). If
#'   `FALSE`, uses the deterministic simplification, which is markedly
#'   conservative. See Details.
#' @param allow_empty Logical. If `FALSE` (the default), an empty prediction
#'   set is replaced by the single most probable class. This is conservative.
#'   Set `TRUE` to return the score inversion exactly.
#' @param seed Optional random seed. Set for the duration of the call only;
#'   the global random stream is restored on exit.
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
                           cal_fraction = 0.5, randomize = TRUE,
                           allow_empty = FALSE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_class(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "classification")

  local_seed(seed)

  split <- split_data(nrow(x), cal_fraction)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  probs_cal <- label_probs(mod$predict_fun(fitted, x_cal), levels(y))
  validate_probs(probs_cal, levels(y), "calibration probability matrix")

  scores <- aps_scores(probs_cal, y_cal, randomize = randomize)
  q <- conformal_quantile(scores, alpha)

  probs_new <- label_probs(mod$predict_fun(fitted, x_new), levels(y))
  validate_probs(probs_new, levels(y), "probability matrix for x_new")

  result <- build_aps_sets(probs_new, q, randomize = randomize,
                           allow_empty = allow_empty)
  warn_saturated(result$sets, levels(y), "APS", randomize)

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
    randomize = randomize,
    allow_empty = allow_empty
  ), class = "predictset_class")
}
