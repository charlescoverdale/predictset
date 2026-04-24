# Plot Method for Regression Conformal Objects

Creates a base R plot showing prediction intervals. Points are ordered
by predicted value, with intervals shown as vertical segments.

## Usage

``` r
# S3 method for class 'predictset_reg'
plot(x, max_points = 200, ...)
```

## Arguments

- x:

  A `predictset_reg` object.

- max_points:

  Maximum number of points to display. Default `200`. If there are more
  predictions, a random subset is shown.

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

The input object, invisibly.

## Examples

``` r
set.seed(42)
x <- matrix(rnorm(200 * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(200)
x_new <- matrix(rnorm(50 * 3), ncol = 3)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
plot(result)

```
