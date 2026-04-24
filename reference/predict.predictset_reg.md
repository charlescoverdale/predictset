# Predict Method for Regression Conformal Objects

Generate prediction intervals for new data using a fitted conformal
prediction object.

## Usage

``` r
# S3 method for class 'predictset_reg'
predict(object, newdata, ...)
```

## Arguments

- object:

  A `predictset_reg` object.

- newdata:

  A numeric matrix or data frame of new predictor variables.

- ...:

  Additional arguments. For Mondrian objects, pass `groups_new` (a
  factor or character vector of group labels for each observation in
  `newdata`).

## Value

A data frame with columns `pred`, `lower`, and `upper`.

## Examples

``` r
set.seed(42)
x <- matrix(rnorm(200 * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(200)
x_new <- matrix(rnorm(10 * 3), ncol = 3)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
preds <- predict(result, newdata = matrix(rnorm(5 * 3), ncol = 3))
```
