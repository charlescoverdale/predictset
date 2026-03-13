#' Create a Model Specification for Conformal Prediction
#'
#' Defines how to train a model and generate predictions, allowing any model
#' to be used with conformal prediction methods.
#'
#' @param train_fun A function with signature `function(x, y)` that takes a
#'   numeric matrix `x` and response `y` (numeric for regression, factor for
#'   classification) and returns a fitted model object.
#' @param predict_fun A function with signature `function(object, x_new)` that
#'   takes a fitted model object and a numeric matrix `x_new` and returns
#'   predictions. For regression, must return a numeric vector. For
#'   classification, must return a probability matrix with columns named by
#'   class labels.
#' @param type Character string, either `"regression"` or `"classification"`.
#'
#' @return A `predictset_model` object (a list with components `train_fun`,
#'   `predict_fun`, and `type`).
#'
#' @examples
#' reg_model <- make_model(
#'   train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
#'   predict_fun = function(object, x_new) {
#'     predict(object, newdata = as.data.frame(x_new))
#'   },
#'   type = "regression"
#' )
#'
#' @export
make_model <- function(train_fun, predict_fun,
                       type = c("regression", "classification")) {
  type <- match.arg(type)

  if (!is.function(train_fun)) {
    cli_abort("{.arg train_fun} must be a function.")
  }
  if (!is.function(predict_fun)) {
    cli_abort("{.arg predict_fun} must be a function.")
  }

  structure(
    list(
      train_fun = train_fun,
      predict_fun = predict_fun,
      type = type
    ),
    class = "predictset_model"
  )
}

# Resolve a model argument into a predictset_model object
# Handles: predictset_model, formula, fitted lm/glm/ranger/etc.
resolve_model <- function(model, type = "regression") {
  if (inherits(model, "predictset_model")) {
    return(model)
  }

  if (inherits(model, "formula")) {
    return(make_model(
      train_fun = function(x, y) {
        lm(y ~ ., data = data.frame(y = y, x))
      },
      predict_fun = function(object, x_new) {
        as.numeric(predict(object, newdata = as.data.frame(x_new)))
      },
      type = type
    ))
  }

  # Auto-detect fitted model objects
  if (inherits(model, "lm") || inherits(model, "glm")) {
    return(auto_lm(type))
  }

  if (inherits(model, "ranger")) {
    return(auto_ranger(type))
  }

  # Fallback: try generic predict
  return(make_model(
    train_fun = function(x, y) {
      cli_abort("Cannot auto-detect training for this model type. Use {.fn make_model}.")
    },
    predict_fun = function(object, x_new) {
      as.numeric(predict(object, newdata = as.data.frame(x_new)))
    },
    type = type
  ))
}

auto_lm <- function(type) {
  if (type == "classification") {
    make_model(
      train_fun = function(x, y) {
        glm(y ~ ., data = data.frame(y = y, x), family = "binomial")
      },
      predict_fun = function(object, x_new) {
        p <- predict(object, newdata = as.data.frame(x_new), type = "response")
        cbind(1 - p, p)
      },
      type = "classification"
    )
  } else {
    make_model(
      train_fun = function(x, y) {
        lm(y ~ ., data = data.frame(y = y, x))
      },
      predict_fun = function(object, x_new) {
        as.numeric(predict(object, newdata = as.data.frame(x_new)))
      },
      type = "regression"
    )
  }
}

auto_ranger <- function(type) {
  if (type == "classification") {
    make_model(
      train_fun = function(x, y) {
        if (!requireNamespace("ranger", quietly = TRUE)) {
          cli_abort("Package {.pkg ranger} is required.")
        }
        ranger::ranger(y ~ ., data = data.frame(y = y, x), probability = TRUE)
      },
      predict_fun = function(object, x_new) {
        predict(object, data = as.data.frame(x_new))$predictions
      },
      type = "classification"
    )
  } else {
    make_model(
      train_fun = function(x, y) {
        if (!requireNamespace("ranger", quietly = TRUE)) {
          cli_abort("Package {.pkg ranger} is required.")
        }
        ranger::ranger(y ~ ., data = data.frame(y = y, x))
      },
      predict_fun = function(object, x_new) {
        predict(object, data = as.data.frame(x_new))$predictions
      },
      type = "regression"
    )
  }
}

