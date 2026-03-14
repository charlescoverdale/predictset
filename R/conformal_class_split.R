#' Split Conformal Prediction Sets for Classification
#'
#' @description
#' **\[Deprecated\]** `conformal_class_split()` is identical to
#' [conformal_lac()] and is deprecated. Use [conformal_lac()] instead.
#'
#' @inheritParams conformal_lac
#'
#' @return A `predictset_class` object. See [conformal_lac()] for details.
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
#'   train_fun = function(x, y) {
#'     df <- data.frame(y = y, x)
#'     glm(y ~ ., data = df, family = "binomial")
#'   },
#'   predict_fun = function(object, x_new) {
#'     df <- as.data.frame(x_new)
#'     names(df) <- paste0("X", seq_len(ncol(x_new)))
#'     p <- predict(object, newdata = df, type = "response")
#'     cbind(A = 1 - p, B = p)
#'   },
#'   type = "classification"
#' )
#'
#' \donttest{
#' suppressWarnings(
#'   result <- conformal_class_split(x, y, model = clf, x_new = x_new)
#' )
#' }
#'
#' @export
conformal_class_split <- function(x, y, model, x_new, alpha = 0.10,
                                   cal_fraction = 0.5, seed = NULL) {
  cli_warn(c(
    "Please use {.fn conformal_lac} instead of {.fn conformal_class_split}.",
    "i" = "{.fn conformal_class_split} will be removed in a future version."
  ))
  conformal_lac(x, y, model = model, x_new = x_new, alpha = alpha,
                cal_fraction = cal_fraction, seed = seed)
}
