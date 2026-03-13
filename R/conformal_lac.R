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
#' @param seed Optional random seed.
#'
#' @return A `predictset_class` object. See [conformal_class_split()] for
#'   details. The `method` component is `"lac"`.
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
#' @export
conformal_lac <- function(x, y, model, x_new, alpha = 0.10,
                           cal_fraction = 0.5, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_class(y)
  x_new <- validate_x(x_new, "x_new")
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

  scores <- lac_scores(probs_cal, y_cal)
  q <- conformal_quantile(scores, alpha)

  probs_new <- mod$predict_fun(fitted, x_new)
  if (is.null(colnames(probs_new))) {
    colnames(probs_new) <- levels(y)
  }

  result <- build_lac_sets(probs_new, q)

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
    model = mod
  ), class = "predictset_class")
}
