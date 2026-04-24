# Plot Method for ACI Objects

Creates a two-panel base R plot. The top panel shows prediction
intervals over time; the bottom panel shows the adaptive alpha trace.

## Usage

``` r
# S3 method for class 'predictset_aci'
plot(x, max_points = 500, ...)
```

## Arguments

- x:

  A `predictset_aci` object.

- max_points:

  Maximum number of points to display. Default `500`.

- ...:

  Additional arguments (currently unused).

## Value

The input object, invisibly.

## Examples

``` r
set.seed(42)
n <- 100
y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)
y_pred <- c(0, y_true[-n])

# \donttest{
result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)
plot(result)

# }
```
