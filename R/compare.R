#' Compare Conformal Prediction Methods
#'
#' Runs multiple conformal prediction methods on the same data and returns a
#' comparison data frame with coverage, interval width, and computation time
#' for each method.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object, a [make_model()] specification, or a
#'   formula.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param y_new A numeric vector of true response values for `x_new`, used to
#'   compute empirical coverage.
#' @param methods Character vector of method names to compare. Default
#'   `c("split", "cv")`. Available methods: `"split"`, `"cv"`, `"jackknife"`,
#'   `"jackknife_basic"`.
#' @param alpha Miscoverage level. Default `0.10`.
#' @param seed Optional random seed.
#'
#' @return A `predictset_compare` object (a data frame) with columns:
#' \describe{
#'   \item{method}{Character. The method name.}
#'   \item{coverage}{Numeric. Empirical coverage on `y_new`.}
#'   \item{mean_width}{Numeric. Mean interval width.}
#'   \item{median_width}{Numeric. Median interval width.}
#'   \item{time_seconds}{Numeric. Elapsed time in seconds.}
#' }
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(n)
#' x_new <- matrix(rnorm(100 * 3), ncol = 3)
#' y_new <- x_new[, 1] * 2 + rnorm(100)
#'
#' \donttest{
#' comp <- conformal_compare(x, y, model = y ~ ., x_new = x_new,
#'                            y_new = y_new)
#' print(comp)
#' }
#'
#' @family diagnostics
#' @export
conformal_compare <- function(x, y, model, x_new, y_new,
                               methods = c("split", "cv"),
                               alpha = 0.10, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  if (length(y_new) != nrow(x_new)) {
    cli_abort("{.arg y_new} must have the same length as {.code nrow(x_new)} ({nrow(x_new)}).")
  }

  valid_methods <- c("split", "cv", "jackknife", "jackknife_basic")
  bad <- setdiff(methods, valid_methods)
  if (length(bad) > 0) {
    cli_abort("Unknown method{?s}: {.val {bad}}.")
  }

  results <- data.frame(
    method = character(),
    coverage = numeric(),
    mean_width = numeric(),
    median_width = numeric(),
    time_seconds = numeric(),
    stringsAsFactors = FALSE
  )

  for (m in methods) {
    t0 <- proc.time()["elapsed"]

    res <- switch(m,
      split = conformal_split(x, y, model = model, x_new = x_new,
                               alpha = alpha, seed = seed),
      cv = conformal_cv(x, y, model = model, x_new = x_new,
                         alpha = alpha, seed = seed),
      jackknife = conformal_jackknife(x, y, model = model, x_new = x_new,
                                       alpha = alpha, plus = TRUE, seed = seed),
      jackknife_basic = conformal_jackknife(x, y, model = model, x_new = x_new,
                                             alpha = alpha, plus = FALSE,
                                             seed = seed)
    )

    elapsed <- as.numeric(proc.time()["elapsed"] - t0)
    cov <- coverage(res, y_new)
    widths <- interval_width(res)

    results <- rbind(results, data.frame(
      method = m,
      coverage = cov,
      mean_width = mean(widths),
      median_width = stats::median(widths),
      time_seconds = round(elapsed, 3),
      stringsAsFactors = FALSE
    ))
  }

  structure(results, class = c("predictset_compare", "data.frame"))
}

#' @export
print.predictset_compare <- function(x, ...) {
  cli_h1("Conformal Method Comparison")
  cli_text("")

  for (i in seq_len(nrow(x))) {
    cli_bullets(c(
      "*" = "{.strong {x$method[i]}}: coverage = {.val {round(x$coverage[i], 3)}}, mean width = {.val {round(x$mean_width[i], 3)}}, time = {.val {x$time_seconds[i]}}s"
    ))
  }

  invisible(x)
}
