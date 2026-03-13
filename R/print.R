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
    cqr = "Conformalized Quantile Regression"
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
  } else {
    bullets <- c(bullets,
      "*" = "Conformal quantile: {.val {round(x$quantile, 4)}}"
    )
  }
  bullets <- c(bullets,
    "*" = "Median interval width: {.val {round(median(x$upper - x$lower), 4)}}"
  )
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
    split = "Split Conformal",
    aps = "Adaptive Prediction Sets",
    raps = "Regularized Adaptive Prediction Sets",
    lac = "Least Ambiguous Classifier"
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
