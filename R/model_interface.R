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

# Build the modelling data frame handed to base R model functions. The response
# is always named `y`, since y is supplied separately from x.
model_frame <- function(x, y) {
  data.frame(y = y, as.data.frame(x, stringsAsFactors = FALSE))
}

# Rewrite a user formula so the response is `y`, preserving the right-hand
# side exactly as written. `resp ~ a + log(b)` becomes `y ~ a + log(b)`.
normalise_formula <- function(f) {
  rhs <- paste(deparse(f[[length(f)]]), collapse = " ")
  stats::as.formula(paste("y ~", rhs), env = environment(f))
}

# Check that every variable the formula needs is available in x.
check_formula_vars <- function(f, x, what = "formula") {
  vars <- setdiff(all.vars(f), c("y", "."))
  missing <- setdiff(vars, colnames(x))
  if (length(missing) > 0) {
    cli_abort(c(
      "The {what} refers to variable{?s} not present in {.arg x}: {.val {missing}}.",
      "i" = "Available column{?s}: {.val {colnames(x)}}."
    ))
  }
  invisible(NULL)
}

# Probability matrix from a binomial glm, with class-label column names and the
# usual convention that the second factor level is the modelled ("success")
# class. The levels are recorded on the fitted object at training time rather
# than recovered from it, so the columns are always labelled correctly.
glm_binomial_probs <- function(object, x_new) {
  classes <- attr(object, "predictset_classes")
  p <- stats::predict(object, newdata = as.data.frame(x_new), type = "response")
  probs <- cbind(1 - as.numeric(p), as.numeric(p))
  colnames(probs) <- classes
  probs
}

tag_classes <- function(fit, y) {
  attr(fit, "predictset_classes") <- levels(y)
  fit
}

formula_model <- function(f, type, classes = NULL) {
  f <- normalise_formula(f)

  if (type == "classification") {
    make_model(
      train_fun = function(x, y) {
        check_formula_vars(f, x)
        if (nlevels(y) != 2) {
          cli_abort(c(
            "A formula can only be used for two-class classification, but {.arg y} has {nlevels(y)} levels.",
            "i" = "Supply a multiclass model with {.fn make_model}."
          ))
        }
        tag_classes(stats::glm(f, data = model_frame(x, y), family = "binomial"), y)
      },
      predict_fun = function(object, x_new) {
        glm_binomial_probs(object, x_new)
      },
      type = "classification"
    )
  } else {
    make_model(
      train_fun = function(x, y) {
        check_formula_vars(f, x)
        stats::lm(f, data = model_frame(x, y))
      },
      predict_fun = function(object, x_new) {
        as.numeric(stats::predict(object, newdata = as.data.frame(x_new)))
      },
      type = "regression"
    )
  }
}

# Refit a user-supplied fitted model on new training data, preserving the
# original formula and every argument of the original call. Anything the
# refit cannot reproduce would silently change the model, so it errors instead.
refit_model <- function(model, type) {
  if (inherits(model, "ranger")) {
    return(refit_ranger(model, type))
  }

  f <- normalise_formula(stats::formula(model))
  is_classification <- type == "classification"

  if (is_classification &&
      !(inherits(model, "glm") && identical(model$family$family, "binomial"))) {
    cli_abort(c(
      "Cannot derive class probabilities from a fitted {.cls {class(model)[1]}} object.",
      "i" = "Classification needs a model that returns a probability matrix.
             Use a binomial {.fn glm}, a {.pkg ranger} model fitted with
             {.code probability = TRUE}, or wrap yours with {.fn make_model}."
    ))
  }

  make_model(
    train_fun = function(x, y) {
      check_formula_vars(f, x, what = "fitted model")
      refit <- try(
        stats::update(model, formula. = f, data = model_frame(x, y)),
        silent = TRUE
      )
      if (inherits(refit, "try-error")) {
        cli_abort(c(
          "Could not refit the supplied {.cls {class(model)[1]}} model on the conformal training split.",
          "x" = conditionMessage(attr(refit, "condition")),
          "i" = "Wrap the model with {.fn make_model} instead."
        ))
      }
      if (is_classification) refit <- tag_classes(refit, y)
      refit
    },
    predict_fun = if (is_classification) {
      function(object, x_new) glm_binomial_probs(object, x_new)
    } else {
      function(object, x_new) {
        as.numeric(stats::predict(object, newdata = as.data.frame(x_new)))
      }
    },
    type = type
  )
}

# ranger objects are not updatable, but they carry their originating call, so
# re-evaluating it with new data preserves num.trees, mtry, and the rest.
refit_ranger <- function(model, type) {
  cl <- model$call
  f <- normalise_formula(stats::formula(model))

  if (type == "classification" && !isTRUE(model$treetype == "Probability estimation")) {
    cli_abort(c(
      "Classification requires a {.pkg ranger} model fitted with {.code probability = TRUE}.",
      "i" = "Refit as {.code ranger(y ~ ., data = ..., probability = TRUE)}."
    ))
  }

  make_model(
    train_fun = function(x, y) {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        cli_abort("Package {.pkg ranger} is required.")
      }
      check_formula_vars(f, x, what = "fitted model")
      cl[[1]] <- quote(ranger::ranger)
      cl$formula <- f
      cl$data <- model_frame(x, y)
      eval(cl, envir = environment(f))
    },
    predict_fun = function(object, x_new) {
      stats::predict(object, data = as.data.frame(x_new))$predictions
    },
    type = type
  )
}

# Resolve a model argument into a predictset_model object.
# Handles: predictset_model, formula, and fitted objects that can be refitted.
resolve_model <- function(model, type = "regression") {
  if (inherits(model, "predictset_model")) {
    if (!identical(model$type, type)) {
      cli_abort(c(
        "{.arg model} was created with {.code type = \"{model$type}\"} but a {type} method was called.",
        "i" = "Rebuild it with {.code make_model(..., type = \"{type}\")}."
      ))
    }
    return(model)
  }

  if (inherits(model, "formula")) {
    return(formula_model(model, type))
  }

  # Any fitted object carrying a formula and an update()-able call can be
  # refitted on each conformal training split.
  if (is.object(model) && !is.data.frame(model)) {
    f <- try(stats::formula(model), silent = TRUE)
    if (!inherits(f, "try-error") && inherits(f, "formula")) {
      return(refit_model(model, type))
    }
  }

  cli_abort(c(
    "Cannot derive a training function from an object of class {.cls {class(model)[1]}}.",
    "i" = "Conformal prediction refits the model on each split, so it needs a
           training function as well as a prediction function.",
    "i" = "Wrap it with {.fn make_model}."
  ))
}
