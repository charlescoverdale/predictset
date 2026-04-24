# Jackknife+ Conformal Prediction Intervals

Constructs prediction intervals using the Jackknife+ method of Barber et
al. (2021). Uses leave-one-out models to form prediction intervals with
finite-sample coverage guarantees.

## Usage

``` r
conformal_jackknife(
  x,
  y,
  model,
  x_new = NULL,
  alpha = 0.1,
  plus = TRUE,
  verbose = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictor variables.

- y:

  A numeric vector of response values.

- model:

  A fitted model object (e.g., from
  [`lm()`](https://rdrr.io/r/stats/lm.html)), a
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification, or a formula (which will fit a linear model).

- x_new:

  A numeric matrix or data frame of new predictor variables. If `NULL`,
  intervals are computed for the training data.

- alpha:

  Miscoverage level. Default `0.10` gives 90 percent prediction
  intervals.

- plus:

  Logical. If `TRUE` (default), uses the Jackknife+ method. If `FALSE`,
  uses the basic jackknife.

- verbose:

  Logical. If `TRUE`, shows a progress bar during LOO fitting. Default
  `FALSE`.

- seed:

  Optional random seed for reproducible data splitting.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"jackknife_plus"` or
`"jackknife"`. Additional components include `loo_models` (list of n
leave-one-out fitted models) and `loo_residuals` (numeric vector of LOO
absolute residuals).

## Details

The Jackknife+ method fits n leave-one-out models and uses each model's
prediction at the test point, shifted by the corresponding LOO residual,
to construct the interval. This is distinct from basic jackknife, which
centres a single full-model prediction and adds a quantile of the LOO
residuals.

The Jackknife+ theoretical coverage guarantee is \\1 - 2\alpha\\, not
\\1 - \alpha\\ (Barber et al. 2021, Theorem 1). This is weaker than
split conformal's \\1 - \alpha\\ guarantee. In practice, Jackknife+
coverage is typically much closer to \\1 - \alpha\\.

## References

Barber, R.F., Candes, E.J., Ramdas, A. and Tibshirani, R.J. (2021).
Predictive inference with the jackknife+. *Annals of Statistics*, 49(1),
486–507.

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
[`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md),
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md),
[`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)

## Examples

``` r
set.seed(42)
n <- 50
x <- matrix(rnorm(n * 2), ncol = 2)
y <- x[, 1] + rnorm(n)
x_new <- matrix(rnorm(10 * 2), ncol = 2)

# \donttest{
result <- conformal_jackknife(x, y, model = y ~ ., x_new = x_new)
print(result)
#> 
#> ── Conformal Prediction Intervals (Jackknife+) ─────────────────────────────────
#> • Coverage target: "90%"
#> • Training: 50 | Calibration: 50 | Predictions: 10
#> • Median LOO residual: 0.4319
#> • Median interval width: 3.0956
# }
```
