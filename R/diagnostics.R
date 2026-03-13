#' Empirical Coverage Rate
#'
#' Computes the fraction of true values that fall within the prediction
#' intervals (regression) or prediction sets (classification).
#'
#' @param object A `predictset_reg` or `predictset_class` object.
#' @param y_true A numeric vector (regression) or factor/character vector
#'   (classification) of true values, with the same length as the number of
#'   predictions.
#'
#' @return A single numeric value between 0 and 1 representing the empirical
#'   coverage rate.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(100 * 3), ncol = 3)
#' y_new <- x_new[, 1] * 2 + rnorm(100)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' coverage(result, y_new)
#'
#' @export
coverage <- function(object, y_true) {
  UseMethod("coverage")
}

#' @export
coverage.predictset_reg <- function(object, y_true) {
  if (length(y_true) != length(object$pred)) {
    cli_abort("{.arg y_true} must have the same length as the number of predictions ({length(object$pred)}).")
  }
  mean(y_true >= object$lower & y_true <= object$upper)
}

#' @export
coverage.predictset_class <- function(object, y_true) {
  y_true <- as.character(y_true)
  if (length(y_true) != length(object$sets)) {
    cli_abort("{.arg y_true} must have the same length as the number of predictions ({length(object$sets)}).")
  }
  covered <- vapply(seq_along(y_true), function(i) {
    y_true[i] %in% object$sets[[i]]
  }, logical(1))
  mean(covered)
}

#' Prediction Interval Widths
#'
#' Returns the width of each prediction interval.
#'
#' @param object A `predictset_reg` object.
#'
#' @return A numeric vector of interval widths.
#'
#' @examples
#' set.seed(42)
#' x <- matrix(rnorm(200 * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(200)
#' x_new <- matrix(rnorm(50 * 3), ncol = 3)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' widths <- interval_width(result)
#' summary(widths)
#'
#' @export
interval_width <- function(object) {
  if (!inherits(object, "predictset_reg")) {
    cli_abort("{.fn interval_width} requires a {.cls predictset_reg} object.")
  }
  object$upper - object$lower
}

#' Prediction Set Sizes
#'
#' Returns the number of classes in each prediction set.
#'
#' @param object A `predictset_class` object.
#'
#' @return An integer vector of set sizes.
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' y <- factor(ifelse(x[,1] > 0, "A", "B"))
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
#' sizes <- set_size(result)
#' table(sizes)
#'
#' @export
set_size <- function(object) {
  if (!inherits(object, "predictset_class")) {
    cli_abort("{.fn set_size} requires a {.cls predictset_class} object.")
  }
  vapply(object$sets, length, integer(1))
}
