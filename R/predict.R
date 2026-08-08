#' Predict Method for Regression Conformal Objects
#'
#' Generate prediction intervals for new data using a fitted conformal
#' prediction object.
#'
#' @param object A `predictset_reg` object.
#' @param newdata A numeric matrix or data frame of new predictor variables.
#' @param ... Additional arguments. For Mondrian objects, pass
#'   `groups_new` (a factor or character vector of group labels for each
#'   observation in `newdata`). For weighted conformal objects, pass
#'   `weights_new` (importance weights for each row of `newdata`).
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
  dots <- list(...)

  if (object$method == "cqr") {
    lo_pred <- object$model$lower$predict_fun(object$fitted_model$lower, newdata)
    hi_pred <- object$model$upper$predict_fun(object$fitted_model$upper, newdata)
    pred <- (lo_pred + hi_pred) / 2
    lower <- lo_pred - object$quantile
    upper <- hi_pred + object$quantile
  } else if (object$method == "mondrian") {
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    groups_new <- check_groups_new(dots$groups_new, nrow(newdata))
    q_i <- vapply(groups_new, group_quantile_for, numeric(1),
                  group_quantiles = object$group_quantiles,
                  pooled_q = object$quantile)
    lower <- pred - q_i
    upper <- pred + q_i
  } else if (object$method == "weighted") {
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    q <- weighted_predict_quantile(object, dots$weights_new, nrow(newdata))
    lower <- pred - q
    upper <- pred + q
  } else if (object$method == "jackknife_plus") {
    # Jackknife+: use stored LOO models and residuals
    pred <- object$model$predict_fun(object$fitted_model, newdata)
    intervals <- jackknife_plus_intervals(newdata, object$model,
                                          object$loo_models,
                                          object$loo_residuals,
                                          object$alpha,
                                          length(object$loo_models))
    lower <- intervals$lower
    upper <- intervals$upper
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
#' @param ... Additional arguments. For Mondrian objects, pass
#'   `groups_new` (a factor or character vector of group labels for each
#'   observation in `newdata`).
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
  dots <- list(...)

  probs_new <- label_probs(
    object$model$predict_fun(object$fitted_model, newdata),
    object$classes
  )
  validate_probs(probs_new, object$classes, "predicted probability matrix")

  randomize <- isTRUE(object$randomize)
  allow_empty <- isTRUE(object$allow_empty)

  if (object$method == "mondrian") {
    groups_new <- check_groups_new(dots$groups_new, nrow(newdata))
    classes <- colnames(probs_new)
    n_new <- nrow(newdata)
    sets <- vector("list", n_new)
    set_probs <- vector("list", n_new)
    for (i in seq_len(n_new)) {
      q_i <- group_quantile_for(groups_new[i], object$group_quantiles,
                                object$quantile)
      p <- probs_new[i, ]
      included <- non_empty(classes[p >= 1 - q_i], classes, p, allow_empty)
      sets[[i]] <- included
      set_probs[[i]] <- setNames(p[included], included)
    }
    result <- list(sets = sets, probs = set_probs)
  } else if (object$method == "aps") {
    result <- build_aps_sets(probs_new, object$quantile,
                             randomize = randomize, allow_empty = allow_empty)
  } else if (object$method == "raps") {
    result <- build_raps_sets(probs_new, object$quantile,
                              k_reg = object$k_reg, lambda = object$lambda,
                              randomize = randomize, allow_empty = allow_empty)
  } else {
    result <- build_lac_sets(probs_new, object$quantile,
                             allow_empty = allow_empty)
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
    model = object$model,
    randomize = randomize,
    allow_empty = allow_empty
  ), class = "predictset_class")
}

# Shared argument handling for the two Mondrian predict branches.
check_groups_new <- function(groups_new, n_new) {
  if (is.null(groups_new)) {
    cli_abort("Mondrian predict requires {.arg groups_new} to be passed via {.code predict(object, newdata, groups_new = ...)}.")
  }
  groups_new <- as.factor(groups_new)
  if (length(groups_new) != n_new) {
    cli_abort("{.arg groups_new} must have length equal to {.code nrow(newdata)} ({n_new}).")
  }
  groups_new
}

# Weighted conformal needs the test-point weights to reproduce its per-point
# quantile. Without them it can only reuse the summary quantile from fitting.
weighted_predict_quantile <- function(object, weights_new, n_new) {
  if (is.null(weights_new)) {
    if (length(unique(object$quantile_by_point)) > 1) {
      cli_warn(c(
        "{.arg weights_new} not supplied to {.fn predict}.",
        "i" = "Falling back to the median conformal quantile from fitting, which is not the exact weighted procedure."
      ))
    }
    return(rep(object$quantile, n_new))
  }
  w_test <- check_weights(weights_new, n_new, "weights_new")
  weighted_conformal_quantile(object$scores, object$cal_weights,
                              object$alpha, w_test)
}
