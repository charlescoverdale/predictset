#' Predict Method for Regression Conformal Objects
#'
#' Generate prediction intervals for new data using a fitted conformal
#' prediction object.
#'
#' @param object A `predictset_reg` object.
#' @param newdata A numeric matrix or data frame of new predictor variables.
#' @param ... Additional arguments. For Mondrian objects, pass
#'   `groups_new` (a factor or character vector of group labels for each
#'   observation in `newdata`).
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
    groups_new <- dots$groups_new
    if (is.null(groups_new)) {
      cli_abort("Mondrian predict requires {.arg groups_new} to be passed via {.code predict(object, newdata, groups_new = ...)}.")
    }
    groups_new <- as.factor(groups_new)
    if (length(groups_new) != nrow(newdata)) {
      cli_abort("{.arg groups_new} must have length equal to {.code nrow(newdata)} ({nrow(newdata)}).")
    }
    n_new <- nrow(newdata)
    lower <- numeric(n_new)
    upper <- numeric(n_new)
    for (i in seq_len(n_new)) {
      g <- as.character(groups_new[i])
      if (g %in% names(object$group_quantiles)) {
        q_i <- object$group_quantiles[g]
      } else {
        q_i <- object$quantile
      }
      lower[i] <- pred[i] - q_i
      upper[i] <- pred[i] + q_i
    }
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

  probs_new <- object$model$predict_fun(object$fitted_model, newdata)
  if (is.null(colnames(probs_new))) {
    colnames(probs_new) <- object$classes
  }

  # Validate probability matrix
  missing_cls <- setdiff(object$classes, colnames(probs_new))
  if (length(missing_cls) > 0) {
    cli_abort(
      "Predicted probability matrix is missing columns for class{?es}: {.val {missing_cls}}."
    )
  }
  if (any(probs_new < 0 | probs_new > 1, na.rm = TRUE)) {
    cli_abort("Predicted probabilities must be in [0, 1].")
  }
  row_sums <- rowSums(probs_new)
  if (any(abs(row_sums - 1) > 0.01, na.rm = TRUE)) {
    cli_warn("Some rows of the predicted probability matrix do not sum to 1.")
  }

  if (object$method == "mondrian") {
    groups_new <- dots$groups_new
    if (is.null(groups_new)) {
      cli_abort("Mondrian predict requires {.arg groups_new} to be passed via {.code predict(object, newdata, groups_new = ...)}.")
    }
    groups_new <- as.factor(groups_new)
    if (length(groups_new) != nrow(newdata)) {
      cli_abort("{.arg groups_new} must have length equal to {.code nrow(newdata)} ({nrow(newdata)}).")
    }
    n_new <- nrow(newdata)
    classes <- colnames(probs_new)
    sets <- vector("list", n_new)
    set_probs <- vector("list", n_new)
    for (i in seq_len(n_new)) {
      g <- as.character(groups_new[i])
      if (g %in% names(object$group_quantiles)) {
        q_i <- object$group_quantiles[g]
      } else {
        q_i <- object$quantile
      }
      p <- probs_new[i, ]
      included <- classes[p >= 1 - q_i]
      if (length(included) == 0) {
        included <- classes[which.max(p)]
      }
      sets[[i]] <- included
      set_probs[[i]] <- setNames(p[included], included)
    }
    result <- list(sets = sets, probs = set_probs)
  } else if (object$method %in% c("aps")) {
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
    model = object$model,
    randomize = object$randomize
  ), class = "predictset_class")
}
