# Shared test fixtures

make_regression_data <- function(n = 500, p = 5, seed = 123) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("X", seq_len(p))
  y <- x[, 1] * 2 + x[, 2] + rnorm(n, sd = 0.5)
  list(x = x, y = y)
}

make_classification_data <- function(n = 500, p = 5, n_classes = 3, seed = 123) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), ncol = p)
  colnames(x) <- paste0("X", seq_len(p))
  probs <- exp(x[, seq_len(min(p, n_classes))])
  probs <- probs / rowSums(probs)
  classes <- LETTERS[seq_len(n_classes)]
  y <- factor(
    apply(probs, 1, function(p) sample(classes, 1, prob = p)),
    levels = classes
  )
  list(x = x, y = y)
}

test_reg_model <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(object, x_new) {
    as.numeric(predict(object, newdata = as.data.frame(x_new)))
  },
  type = "regression"
)

test_class_model_binary <- make_model(
  train_fun = function(x, y) {
    glm(y ~ ., data = data.frame(y = y, x), family = "binomial")
  },
  predict_fun = function(object, x_new) {
    p <- predict(object, newdata = as.data.frame(x_new), type = "response")
    cbind(A = 1 - p, B = p)
  },
  type = "classification"
)

make_multiclass_model <- function() {
  make_model(
    train_fun = function(x, y) {
      # Simple multinomial via separate binomials (for testing only)
      classes <- levels(y)
      models <- list()
      for (cls in classes) {
        y_bin <- as.integer(y == cls)
        models[[cls]] <- glm(y_bin ~ ., data = data.frame(y_bin = y_bin, x),
                             family = "binomial")
      }
      list(models = models, classes = classes)
    },
    predict_fun = function(object, x_new) {
      df <- as.data.frame(x_new)
      probs <- sapply(object$models, function(mod) {
        predict(mod, newdata = df, type = "response")
      })
      # Normalize rows to sum to 1
      probs <- probs / rowSums(probs)
      colnames(probs) <- object$classes
      probs
    },
    type = "classification"
  )
}
