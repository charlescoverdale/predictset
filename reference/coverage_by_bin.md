# Empirical Coverage by Prediction Bin

Bins predictions into quantile-based groups and computes coverage within
each bin. Useful for detecting systematic under- or over-coverage as a
function of predicted value.

## Usage

``` r
coverage_by_bin(object, y_true, bins = 10)
```

## Arguments

- object:

  A `predictset_reg` object.

- y_true:

  A numeric vector of true response values.

- bins:

  Number of bins. Default `10`.

## Value

A data frame with columns `bin`, `coverage`, `n`, and `mean_width`.

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`interval_width()`](https://charlescoverdale.github.io/predictset/reference/interval_width.md),
[`set_size()`](https://charlescoverdale.github.io/predictset/reference/set_size.md)

## Examples

``` r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(200)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
coverage_by_bin(result, y_new, bins = 5)
#>   bin coverage  n mean_width
#> 1   1    0.925 40      3.388
#> 2   2    0.875 40      3.388
#> 3   3    0.875 40      3.388
#> 4   4    0.875 40      3.388
#> 5   5    0.900 40      3.388
```
