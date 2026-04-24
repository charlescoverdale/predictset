# Conformal P-Values

Computes conformal p-values for new observations given calibration
nonconformity scores. The p-value indicates how conforming a new
observation is relative to the calibration set.

## Usage

``` r
conformal_pvalue(scores, new_scores)
```

## Arguments

- scores:

  A numeric vector of calibration nonconformity scores.

- new_scores:

  A numeric vector of nonconformity scores for new observations.

## Value

A numeric vector of p-values, one per element of `new_scores`. Each
p-value is in (0, 1\].

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`interval_width()`](https://charlescoverdale.github.io/predictset/reference/interval_width.md),
[`set_size()`](https://charlescoverdale.github.io/predictset/reference/set_size.md)

## Examples

``` r
# Calibration scores from a conformal split
set.seed(42)
cal_scores <- abs(rnorm(100))
new_scores <- abs(rnorm(5))

pvals <- conformal_pvalue(cal_scores, new_scores)
print(pvals)
#> [1] 0.25742574 0.29702970 0.32673267 0.07920792 0.49504950
```
