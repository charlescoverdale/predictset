#' Print Method for Regression Conformal Objects
#'
#' @param x A `predictset_reg` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.predictset_reg <- function(x, ...) {
  method_names <- c(
    split = "Split Conformal",
    cv_plus = "CV+",
    jackknife_plus = "Jackknife+",
    jackknife = "Jackknife",
    cqr = "Conformalized Quantile Regression",
    mondrian = "Mondrian (Group-Conditional)",
    weighted = "Weighted Conformal"
  )
  method_label <- method_names[x$method]

  cli_h1("Conformal Prediction Intervals ({method_label})")
  bullets <- c(
    "*" = "Coverage target: {.val {(1 - x$alpha) * 100}%}",
    "*" = "Training: {.val {x$n_train}} | Calibration: {.val {x$n_cal}} | Predictions: {.val {length(x$pred)}}"
  )
  if (x$method == "cv_plus") {
    bullets <- c(bullets,
      "*" = "Median residual: {.val {round(stats::median(x$residuals), 4)}}"
    )
  } else if (x$method == "jackknife_plus") {
    bullets <- c(bullets,
      "*" = "Median LOO residual: {.val {round(stats::median(x$loo_residuals), 4)}}"
    )
  } else if (x$method == "weighted" &&
             length(unique(x$quantile_by_point)) > 1) {
    bullets <- c(bullets,
      "*" = "Conformal quantile: {.val {round(min(x$quantile_by_point), 4)}} to {.val {round(max(x$quantile_by_point), 4)}} across test points (median {.val {round(x$quantile, 4)}})"
    )
  } else {
    bullets <- c(bullets,
      "*" = "Conformal quantile: {.val {round(x$quantile, 4)}}"
    )
  }
  bullets <- c(bullets,
    "*" = "Median interval width: {.val {round(median(x$upper - x$lower), 4)}}"
  )
  if (isTRUE(x$train_approximation)) {
    bullets <- c(bullets,
      "!" = "Intervals are for the training data and use an approximation without the CV+ guarantee. Supply {.arg x_new} for exact CV+ intervals."
    )
  }
  if (any(!is.finite(x$upper - x$lower))) {
    bullets <- c(bullets,
      "!" = "{sum(!is.finite(x$upper - x$lower))} interval{?s} {?is/are} unbounded: too few calibration points for {.code alpha = {x$alpha}}."
    )
  }
  cli_bullets(bullets)
  invisible(x)
}

#' Print Method for Classification Conformal Objects
#'
#' @param x A `predictset_class` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.predictset_class <- function(x, ...) {
  method_names <- c(
    aps = "Adaptive Prediction Sets",
    raps = "Regularized Adaptive Prediction Sets",
    lac = "Least Ambiguous Classifier",
    mondrian = "Mondrian (Group-Conditional)"
  )
  method_label <- method_names[x$method]

  sizes <- vapply(x$sets, length, integer(1))

  cli_h1("Conformal Prediction Sets ({method_label})")
  cli_bullets(c(
    "*" = "Coverage target: {.val {(1 - x$alpha) * 100}%}",
    "*" = "Classes: {.val {paste(x$classes, collapse = ', ')}}",
    "*" = "Training: {.val {x$n_train}} | Calibration: {.val {x$n_cal}} | Predictions: {.val {length(x$sets)}}",
    "*" = "Median set size: {.val {median(sizes)}} | Mean set size: {.val {round(mean(sizes), 2)}}"
  ))
  invisible(x)
}

#' Print Method for ACI Objects
#'
#' @param x A `predictset_aci` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.predictset_aci <- function(x, ...) {
  cli_h1("Adaptive Conformal Inference")
  cli_bullets(c(
    "*" = "Target coverage: {.val {(1 - x$alpha) * 100}%}",
    "*" = "Observations: {.val {x$n}}",
    "*" = "Learning rate (gamma): {.val {x$gamma}}",
    "*" = "Empirical coverage: {.val {round(x$coverage * 100, 1)}%}",
    "*" = "Alpha range: [{.val {round(min(x$alphas), 4)}}, {.val {round(max(x$alphas), 4)}}]"
  ))
  invisible(x)
}

#' Print Method for Model Specifications
#'
#' @param x A `predictset_model` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.predictset_model <- function(x, ...) {
  cli_h1("Conformal Prediction Model Specification")
  cli_bullets(c(
    "*" = "Type: {.val {x$type}}"
  ))
  invisible(x)
}
