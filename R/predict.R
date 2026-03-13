#' Predict Method for Regression Conformal Objects
#'
#' Generate prediction intervals for new data using a fitted conformal
#' prediction object.
#'
#' @param object A `predictset_reg` object.
#' @param newdata A numeric matrix or data frame of new predictor variables.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `pred`, `lower`, and `upper`.
#'
#' @examples
#' set.seed(42)
#' x <- matrix(rnorm(200 * 3), ncol = 3)
#' y <- x[, 1] * 2 + rnorm(200)
#' x_new <- matrix(rnorm(10 * 3), ncol = 3)
#'
#' result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
#' preds <- predict(result, newdata = matrix(rnorm(5 * 3), ncol = 3))
#'
#' @export
predict.predictset_reg <- function(object, newdata, ...) {
  newdata <- validate_x(newdata, "newdata")

  if (object$method == "cqr") {
    lo_pred <- object$model$lower$predict_fun(object$fitted_model$lower, newdata)
    hi_pred <- object$model$upper$predict_fun(object$fitted_model$upper, newdata)
    pred <- (lo_pred + hi_pred) / 2
    lower <- lo_pred - object$quantile
    upper <- hi_pred + object$quantile
  } else if (object$method == "jackknife_plus") {
    # Jackknife+: use stored LOO models and residuals
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    n <- length(object$loo_models)
    n_new <- nrow(newdata)
    lower <- numeric(n_new)
    upper <- numeric(n_new)

    for (j in seq_len(n_new)) {
      loo_preds_at_j <- numeric(n)
      for (i in seq_len(n)) {
        loo_preds_at_j[i] <- object$model$predict_fun(
          object$loo_models[[i]], newdata[j, , drop = FALSE]
        )
      }
      lower_vals <- sort(loo_preds_at_j - object$loo_residuals)
      upper_vals <- sort(loo_preds_at_j + object$loo_residuals)

      k_lo <- floor(object$alpha * (n + 1))
      k_hi <- ceiling((1 - object$alpha) * (n + 1))

      lower[j] <- lower_vals[max(k_lo, 1)]
      upper[j] <- upper_vals[min(k_hi, n)]
    }
  } else if (object$method == "cv_plus") {
    # CV+: use stored fold models, fold_ids, and residuals
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    intervals <- cv_plus_intervals(newdata, object$model, object$fold_models,
                                    object$fold_ids, object$residuals,
                                    object$alpha, object$n_train)
    lower <- intervals$lower
    upper <- intervals$upper
  } else if (!is.null(object$score_type) && object$score_type == "normalized") {
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    sigma <- object$scale_model$predict_fun(object$fitted_scale, newdata)
    sigma <- pmax(sigma, MIN_SCALE)
    lower <- pred - object$quantile * sigma
    upper <- pred + object$quantile * sigma
  } else {
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    lower <- pred - object$quantile
    upper <- pred + object$quantile
  }

  data.frame(pred = pred, lower = lower, upper = upper)
}

#' Predict Method for Classification Conformal Objects
#'
#' Generate prediction sets for new data using a fitted conformal prediction
#' object.
#'
#' @param object A `predictset_class` object.
#' @param newdata A numeric matrix or data frame of new predictor variables.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `predictset_class` object with updated sets and probabilities.
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
#' preds <- predict(result, newdata = matrix(rnorm(5 * 4), ncol = 4))
#'
#' @export
predict.predictset_class <- function(object, newdata, ...) {
  newdata <- validate_x(newdata, "newdata")

  probs_new <- object$model$predict_fun(object$fitted_model, newdata)
  if (is.null(colnames(probs_new))) {
    colnames(probs_new) <- object$classes
  }

  if (object$method %in% c("aps")) {
    result <- build_aps_sets(probs_new, object$quantile)
  } else if (object$method == "raps") {
    result <- build_raps_sets(probs_new, object$quantile,
                               k_reg = object$k_reg, lambda = object$lambda)
  } else {
    result <- build_lac_sets(probs_new, object$quantile)
  }

  structure(list(
    sets = result$sets,
    probs = result$probs,
    alpha = object$alpha,
    method = object$method,
    scores = object$scores,
    quantile = object$quantile,
    classes = object$classes,
    n_cal = object$n_cal,
    n_train = object$n_train,
    fitted_model = object$fitted_model,
    model = object$model
  ), class = "predictset_class")
}
