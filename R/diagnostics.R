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
#' @family diagnostics
#' @export
coverage <- function(object, y_true) {
  UseMethod("coverage")
}

#' @rdname coverage
#' @export
coverage.predictset_reg <- function(object, y_true) {
  if (length(y_true) != length(object$pred)) {
    cli_abort("{.arg y_true} must have the same length as the number of predictions ({length(object$pred)}).")
  }
  mean(y_true >= object$lower & y_true <= object$upper)
}

#' @rdname coverage
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
#' @family diagnostics
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
#' @family diagnostics
#' @export
set_size <- function(object) {
  if (!inherits(object, "predictset_class")) {
    cli_abort("{.fn set_size} requires a {.cls predictset_class} object.")
  }
  vapply(object$sets, length, integer(1))
}

#' Empirical Coverage by Group
#'
#' Computes empirical coverage within each group, useful for diagnosing
#' conditional coverage violations.
#'
#' @param object A `predictset_reg` or `predictset_class` object.
#' @param y_true True values (numeric for regression, factor/character for
#'   classification).
#' @param groups A factor or character vector of group labels with the same
#'   length as the number of predictions.
#'
#' @return A data frame with columns `group`, `coverage`, `n`, and `target`.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(200 * 3), ncol = 3)
#' y_new <- x_new[, 1] * 2 + rnorm(200)
#' groups <- factor(ifelse(x_new[, 1] > 0, "high", "low"))
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' coverage_by_group(result, y_new, groups)
#'
#' @family diagnostics
#' @export
coverage_by_group <- function(object, y_true, groups) {
  groups <- as.factor(groups)

  if (inherits(object, "predictset_reg")) {
    n_pred <- length(object$pred)
  } else if (inherits(object, "predictset_class")) {
    n_pred <- length(object$sets)
  } else {
    cli_abort("{.fn coverage_by_group} requires a {.cls predictset_reg} or {.cls predictset_class} object.")
  }

  if (length(y_true) != n_pred) {
    cli_abort("{.arg y_true} must have the same length as the number of predictions ({n_pred}).")
  }
  if (length(groups) != n_pred) {
    cli_abort("{.arg groups} must have the same length as the number of predictions ({n_pred}).")
  }

  target <- 1 - object$alpha
  result <- data.frame(
    group = character(),
    coverage = numeric(),
    n = integer(),
    target = numeric(),
    stringsAsFactors = FALSE
  )

  for (g in levels(groups)) {
    idx <- which(groups == g)
    if (length(idx) == 0) next

    if (inherits(object, "predictset_reg")) {
      cov_g <- mean(y_true[idx] >= object$lower[idx] & y_true[idx] <= object$upper[idx])
    } else {
      y_char <- as.character(y_true[idx])
      cov_g <- mean(vapply(seq_along(idx), function(j) {
        y_char[j] %in% object$sets[[idx[j]]]
      }, logical(1)))
    }

    result <- rbind(result, data.frame(
      group = g,
      coverage = cov_g,
      n = length(idx),
      target = target,
      stringsAsFactors = FALSE
    ))
  }

  result
}

#' Empirical Coverage by Prediction Bin
#'
#' Bins predictions into quantile-based groups and computes coverage within
#' each bin. Useful for detecting systematic under- or over-coverage as a
#' function of predicted value.
#'
#' @param object A `predictset_reg` object.
#' @param y_true A numeric vector of true response values.
#' @param bins Number of bins. Default `10`.
#'
#' @return A data frame with columns `bin`, `coverage`, `n`, and `mean_width`.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(200 * 3), ncol = 3)
#' y_new <- x_new[, 1] * 2 + rnorm(200)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' coverage_by_bin(result, y_new, bins = 5)
#'
#' @family diagnostics
#' @export
coverage_by_bin <- function(object, y_true, bins = 10) {
  if (!inherits(object, "predictset_reg")) {
    cli_abort("{.fn coverage_by_bin} requires a {.cls predictset_reg} object.")
  }
  if (length(y_true) != length(object$pred)) {
    cli_abort("{.arg y_true} must have the same length as the number of predictions ({length(object$pred)}).")
  }

  n <- length(object$pred)
  bins <- min(bins, n)

  # Create quantile-based bins
  breaks <- stats::quantile(object$pred, probs = seq(0, 1, length.out = bins + 1))
  breaks[1] <- breaks[1] - 1
  breaks[length(breaks)] <- breaks[length(breaks)] + 1
  bin_ids <- as.integer(cut(object$pred, breaks = breaks, include.lowest = TRUE))

  widths <- object$upper - object$lower
  covered <- y_true >= object$lower & y_true <= object$upper

  result <- data.frame(
    bin = integer(),
    coverage = numeric(),
    n = integer(),
    mean_width = numeric(),
    stringsAsFactors = FALSE
  )

  for (b in sort(unique(bin_ids))) {
    idx <- which(bin_ids == b)
    result <- rbind(result, data.frame(
      bin = b,
      coverage = mean(covered[idx]),
      n = length(idx),
      mean_width = mean(widths[idx]),
      stringsAsFactors = FALSE
    ))
  }

  result
}
