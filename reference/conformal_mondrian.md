# Mondrian Conformal Prediction Intervals (Group-Conditional)

Constructs prediction intervals with group-conditional coverage
guarantees. Instead of a single conformal quantile, a separate quantile
is computed for each group, ensuring coverage within each subgroup (e.g.
by gender, region, or model type).

## Usage

``` r
conformal_mondrian(
  x,
  y,
  model,
  x_new,
  groups,
  groups_new,
  alpha = 0.1,
  cal_fraction = 0.5,
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictor variables.

- y:

  A numeric vector of response values.

- model:

  A fitted model object, a
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification, or a formula.

- x_new:

  A numeric matrix or data frame of new predictor variables.

- groups:

  A factor or character vector of group labels for each observation in
  `x`, with length equal to `nrow(x)`.

- groups_new:

  A factor or character vector of group labels for each observation in
  `x_new`, with length equal to `nrow(x_new)`.

- alpha:

  Miscoverage level. Default `0.10`.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- seed:

  Optional random seed.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"mondrian"`. Additional
components include `groups_new` (the group labels for new data) and
`group_quantiles` (named numeric vector of per-group conformal
quantiles).

## References

Vovk, V., Gammerman, A. and Shafer, G. (2005). *Algorithmic Learning in
a Random World*. Springer.

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
[`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md),
[`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)

## Examples

``` r
set.seed(42)
n <- 400
x <- matrix(rnorm(n * 3), ncol = 3)
groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
y <- x[, 1] * 2 + ifelse(groups == "high", 2, 0.5) * rnorm(n)
x_new <- matrix(rnorm(50 * 3), ncol = 3)
groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

# \donttest{
result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
                              groups = groups, groups_new = groups_new)
print(result)
#> 
#> ── Conformal Prediction Intervals (Mondrian (Group-Conditional)) ───────────────
#> • Coverage target: "90%"
#> • Training: 200 | Calibration: 200 | Predictions: 50
#> • Conformal quantile: 2.3299
#> • Median interval width: 6.0854
# }
```
