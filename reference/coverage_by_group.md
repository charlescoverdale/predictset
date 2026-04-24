# Empirical Coverage by Group

Computes empirical coverage within each group, useful for diagnosing
conditional coverage violations.

## Usage

``` r
coverage_by_group(object, y_true, groups)
```

## Arguments

- object:

  A `predictset_reg` or `predictset_class` object.

- y_true:

  True values (numeric for regression, factor/character for
  classification).

- groups:

  A factor or character vector of group labels with the same length as
  the number of predictions.

## Value

A data frame with columns `group`, `coverage`, `n`, and `target`.

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
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
groups <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
coverage_by_group(result, y_new, groups)
#>   group  coverage   n target
#> 1  high 0.8947368  95    0.9
#> 2   low 0.8857143 105    0.9
```
