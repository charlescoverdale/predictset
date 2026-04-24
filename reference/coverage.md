# Empirical Coverage Rate

Computes the fraction of true values that fall within the prediction
intervals (regression) or prediction sets (classification).

## Usage

``` r
coverage(object, y_true)

# S3 method for class 'predictset_reg'
coverage(object, y_true)

# S3 method for class 'predictset_class'
coverage(object, y_true)
```

## Arguments

- object:

  A `predictset_reg` or `predictset_class` object.

- y_true:

  A numeric vector (regression) or factor/character vector
  (classification) of true values, with the same length as the number of
  predictions.

## Value

A single numeric value between 0 and 1 representing the empirical
coverage rate.

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`interval_width()`](https://charlescoverdale.github.io/predictset/reference/interval_width.md),
[`set_size()`](https://charlescoverdale.github.io/predictset/reference/set_size.md)

## Examples

``` r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(100 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(100)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
coverage(result, y_new)
#> [1] 0.93
```
