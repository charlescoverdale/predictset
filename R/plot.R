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
    cqr = "CQR",
    mondrian = "Mondrian",
    weighted = "Weighted Conformal"
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
    lac = "LAC",
    mondrian = "Mondrian"
  )

  graphics::barplot(tab,
          xlab = "Prediction set size",
          ylab = "Count",
          main = paste0(method_names[x$method],
                        " (", (1 - x$alpha) * 100, "% sets)"),
          col = "steelblue", ...)

  invisible(x)
}

#' Plot Method for ACI Objects
#'
#' Creates a two-panel base R plot. The top panel shows prediction intervals
#' over time; the bottom panel shows the adaptive alpha trace.
#'
#' @param x A `predictset_aci` object.
#' @param max_points Maximum number of points to display. Default `500`.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @examples
#' set.seed(42)
#' n <- 100
#' y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)
#' y_pred <- c(0, y_true[-n])
#'
#' \donttest{
#' result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)
#' plot(result)
#' }
#'
#' @export
plot.predictset_aci <- function(x, max_points = 500, ...) {
  n <- x$n
  if (n > max_points) {
    idx <- sort(sample.int(n, max_points))
  } else {
    idx <- seq_len(n)
  }

  old_par <- graphics::par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(old_par))

  # Top panel: intervals
  finite_lower <- x$lower[idx]
  finite_upper <- x$upper[idx]
  finite_lower[!is.finite(finite_lower)] <- NA
  finite_upper[!is.finite(finite_upper)] <- NA

  plot(idx, rep(NA, length(idx)),
       ylim = range(c(finite_lower, finite_upper), na.rm = TRUE),
       xlab = "Time", ylab = "Value",
       main = paste0("ACI (", (1 - x$alpha) * 100, "% intervals)"))
  segments(idx, finite_lower, idx, finite_upper,
           col = grDevices::adjustcolor("steelblue", alpha.f = 0.3))

  # Bottom panel: alpha trace
  plot(idx, x$alphas[idx], type = "l", col = "darkred",
       xlab = "Time", ylab = expression(alpha[t]),
       main = "Adaptive alpha")
  graphics::abline(h = x$alpha, lty = 2, col = "grey50")

  invisible(x)
}
