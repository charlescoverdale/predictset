#' Plot Method for Regression Conformal Objects
#'
#' Creates a base R plot showing prediction intervals. Points are ordered
#' by predicted value, with intervals shown as vertical segments.
#'
#' @param x A `predictset_reg` object.
#' @param max_points Maximum number of points to display. Default `200`.
#'   If there are more predictions, a random subset is shown.
#' @param ... Additional arguments passed to [plot()].
#'
#' @return The input object, invisibly.
#'
#' @examples
#' set.seed(42)
#' x <- matrix(rnorm(200 * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(200)
#' x_new <- matrix(rnorm(50 * 3), ncol = 3)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' plot(result)
#'
#' @export
plot.predictset_reg <- function(x, max_points = 200, ...) {
  n <- length(x$pred)
  if (n > max_points) {
    idx <- sort(sample.int(n, max_points))
  } else {
    idx <- seq_len(n)
  }

  ord <- order(x$pred[idx])
  pred <- x$pred[idx][ord]
  lower <- x$lower[idx][ord]
  upper <- x$upper[idx][ord]

  method_names <- c(
    split = "Split Conformal",
    cv_plus = "CV+",
    jackknife_plus = "Jackknife+",
    jackknife = "Jackknife",
    cqr = "CQR"
  )

  plot(seq_along(pred), pred,
       ylim = range(c(lower, upper)),
       xlab = "Observation (ordered by prediction)",
       ylab = "Predicted value",
       main = paste0(method_names[x$method],
                     " (", (1 - x$alpha) * 100, "% intervals)"),
       pch = 16, cex = 0.5, ...)
  segments(seq_along(pred), lower, seq_along(pred), upper,
           col = grDevices::adjustcolor("steelblue", alpha.f = 0.4))
  points(seq_along(pred), pred, pch = 16, cex = 0.5)

  invisible(x)
}

#' Plot Method for Classification Conformal Objects
#'
#' Creates a barplot showing the distribution of prediction set sizes.
#'
#' @param x A `predictset_class` object.
#' @param ... Additional arguments passed to [barplot()].
#'
#' @return The input object, invisibly.
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
#' plot(result)
#'
#' @export
plot.predictset_class <- function(x, ...) {
  sizes <- vapply(x$sets, length, integer(1))
  tab <- table(factor(sizes, levels = seq_len(length(x$classes))))

  method_names <- c(
    split = "Split Conformal",
    aps = "APS",
    raps = "RAPS",
    lac = "LAC"
  )

  graphics::barplot(tab,
          xlab = "Prediction set size",
          ylab = "Count",
          main = paste0(method_names[x$method],
                        " (", (1 - x$alpha) * 100, "% sets)"),
          col = "steelblue", ...)

  invisible(x)
}
