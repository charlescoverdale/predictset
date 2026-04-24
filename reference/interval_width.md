# Prediction Interval Widths

Returns the width of each prediction interval.

## Usage

``` r
interval_width(object)
```

## Arguments

- object:

  A `predictset_reg` object.

## Value

A numeric vector of interval widths.

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`set_size()`](https://charlescoverdale.github.io/predictset/reference/set_size.md)

## Examples

``` r
set.seed(42)
x <- matrix(rnorm(200 * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(200)
x_new <- matrix(rnorm(50 * 3), ncol = 3)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
widths <- interval_width(result)
summary(widths)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.983   2.983   2.983   2.983   2.983   2.983 
```
