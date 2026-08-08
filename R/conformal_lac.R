#' Least Ambiguous Classifier Prediction Sets
#'
#' Constructs prediction sets using the Least Ambiguous Classifier (LAC)
#' method. Includes all classes whose predicted probability exceeds
#' `1 - q`, where `q` is the conformal quantile of `1 - p(true class)` scores.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A factor (or character/integer vector coerced to factor) of class
#'   labels.
#' @param model A [make_model()] specification with `type = "classification"`,
#'   or a fitted model object that produces class probabilities.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param alpha Miscoverage level. Default `0.10` gives 90 percent prediction sets.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param allow_empty Logical. If `FALSE` (the default), an empty prediction
#'   set is replaced by the single most probable class. LAC admits empty sets
#'   by design (Sadinle, Lei and Wasserman 2019); set `TRUE` to return them.
#'   Suppressing empty sets is conservative, never anti-conservative.
#' @param seed Optional random seed. Set for the duration of the call only;
#'   the global random stream is restored on exit.
#'
#' @return A `predictset_class` object with components:
#' \describe{
#'   \item{sets}{A list of character vectors, one per new observation.}
#'   \item{probs}{A list of named numeric vectors with predicted probabilities
#'     for included classes.}
#'   \item{alpha}{The miscoverage level used.}
#'   \item{method}{Character string `"lac"`.}
#'   \item{scores}{Numeric vector of calibration scores.}
#'   \item{quantile}{The conformal quantile used.}
#'   \item{classes}{Character vector of all class labels.}
#'   \item{n_cal}{Number of calibration observations.}
#'   \item{n_train}{Number of training observations.}
#'   \item{fitted_model}{The fitted model object.}
#'   \item{model}{The `predictset_model` specification.}
#' }
#'
#' @references
#' Sadinle, M., Lei, J. and Wasserman, L. (2019).
#' Least ambiguous set-valued classifiers with bounded error levels.
#' *Journal of the American Statistical Association*, 114(525), 223-234.
#' \doi{10.1080/01621459.2017.1395341}
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' y <- factor(ifelse(x[,1] + x[,2] > 0, "A", "B"))
#' x_new <- matrix(rnorm(50 * 4), ncol = 4)
#'
#' clf <- make_model(
#'   train_fun = function(x, y) glm(y ~ ., data = data.frame(y = y, x),
#'                                   family = "binomial"),
#'   predict_fun = function(object, x_new) {
#'     df <- as.data.frame(x_new)
#'     names(df) <- paste0("X", seq_len(ncol(x_new)))
#'     p <- predict(object, newdata = df, type = "response")
#'     cbind(A = 1 - p, B = p)
#'   },
#'   type = "classification"
#' )
#'
#' result <- conformal_lac(x, y, model = clf, x_new = x_new)
#' print(result)
#'
#' @family classification methods
#' @export
conformal_lac <- function(x, y, model, x_new, alpha = 0.10,
                           cal_fraction = 0.5, allow_empty = FALSE,
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

  local_seed(seed)

  split <- split_data(nrow(x), cal_fraction)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  probs_cal <- label_probs(mod$predict_fun(fitted, x_cal), levels(y))
  validate_probs(probs_cal, levels(y), "calibration probability matrix")

  scores <- lac_scores(probs_cal, y_cal)
  q <- conformal_quantile(scores, alpha)

  probs_new <- label_probs(mod$predict_fun(fitted, x_new), levels(y))
  validate_probs(probs_new, levels(y), "probability matrix for x_new")

  result <- build_lac_sets(probs_new, q, allow_empty = allow_empty)

  structure(list(
    sets = result$sets,
    probs = result$probs,
    alpha = alpha,
    method = "lac",
    scores = scores,
    quantile = q,
    classes = levels(y),
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod,
    allow_empty = allow_empty
  ), class = "predictset_class")
}
