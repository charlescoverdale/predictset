# Create a Model Specification for Conformal Prediction

Defines how to train a model and generate predictions, allowing any
model to be used with conformal prediction methods.

## Usage

``` r
make_model(train_fun, predict_fun, type = c("regression", "classification"))
```

## Arguments

- train_fun:

  A function with signature `function(x, y)` that takes a numeric matrix
  `x` and response `y` (numeric for regression, factor for
  classification) and returns a fitted model object.

- predict_fun:

  A function with signature `function(object, x_new)` that takes a
  fitted model object and a numeric matrix `x_new` and returns
  predictions. For regression, must return a numeric vector. For
  classification, must return a probability matrix with columns named by
  class labels.

- type:

  Character string, either `"regression"` or `"classification"`.

## Value

A `predictset_model` object (a list with components `train_fun`,
`predict_fun`, and `type`).

## Examples

``` r
reg_model <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(object, x_new) {
    predict(object, newdata = as.data.frame(x_new))
  },
  type = "regression"
)
```
