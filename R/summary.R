#' Summary Method for Regression Conformal Objects
#'
#' @param object A `predictset_reg` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
summary.predictset_reg <- function(object, ...) {
  widths <- object$upper - object$lower
  method_names <- c(
    split = "Split Conformal",
    cv_plus = "CV+",
    jackknife_plus = "Jackknife+",
    jackknife = "Jackknife",
    cqr = "Conformalized Quantile Regression"
  )

  cli_h1("Summary: {method_names[object$method]}")
  cli_bullets(c(
    "*" = "Coverage target: {.val {(1 - object$alpha) * 100}%}",
    "*" = "Number of predictions: {.val {length(object$pred)}}",
    "*" = "Training observations: {.val {object$n_train}}",
    "*" = "Calibration observations: {.val {object$n_cal}}"
  ))
  cli_text("")
  cli_text("Interval widths:")
  cli_bullets(c(
    " " = "Min: {.val {round(min(widths), 4)}}",
    " " = "Q1:  {.val {round(quantile(widths, 0.25), 4)}}",
    " " = "Med: {.val {round(median(widths), 4)}}",
    " " = "Q3:  {.val {round(quantile(widths, 0.75), 4)}}",
    " " = "Max: {.val {round(max(widths), 4)}}"
  ))
  cli_text("")
  cli_text("Calibration scores:")
  cli_bullets(c(
    " " = "Min: {.val {round(min(object$scores), 4)}}",
    " " = "Med: {.val {round(median(object$scores), 4)}}",
    " " = "Max: {.val {round(max(object$scores), 4)}}",
    " " = "Conformal quantile: {.val {round(object$quantile, 4)}}"
  ))
  invisible(object)
}

#' Summary Method for Classification Conformal Objects
#'
#' @param object A `predictset_class` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
summary.predictset_class <- function(object, ...) {
  sizes <- vapply(object$sets, length, integer(1))
  method_names <- c(
    split = "Split Conformal",
    aps = "Adaptive Prediction Sets",
    raps = "Regularized Adaptive Prediction Sets",
    lac = "Least Ambiguous Classifier"
  )

  cli_h1("Summary: {method_names[object$method]}")
  cli_bullets(c(
    "*" = "Coverage target: {.val {(1 - object$alpha) * 100}%}",
    "*" = "Classes: {.val {paste(object$classes, collapse = ', ')}}",
    "*" = "Number of predictions: {.val {length(object$sets)}}",
    "*" = "Training observations: {.val {object$n_train}}",
    "*" = "Calibration observations: {.val {object$n_cal}}"
  ))
  cli_text("")
  cli_text("Set sizes:")
  cli_bullets(c(
    " " = "Min: {.val {min(sizes)}}",
    " " = "Med: {.val {median(sizes)}}",
    " " = "Max: {.val {max(sizes)}}",
    " " = "Mean: {.val {round(mean(sizes), 2)}}"
  ))

  # Distribution table
  tab <- table(sizes)
  cli_text("")
  cli_text("Set size distribution:")
  for (s in names(tab)) {
    pct <- round(100 * tab[s] / length(sizes), 1)
    cli_bullets(c(" " = "Size {s}: {.val {tab[s]}} ({pct}%)"))
  }

  invisible(object)
}
