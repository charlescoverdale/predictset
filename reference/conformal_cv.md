# CV+ Conformal Prediction Intervals

Constructs prediction intervals using the CV+ method of Barber et al.
(2021). Cross-validation residuals and fold-specific models are used to
form observation-specific prediction intervals with finite-sample
coverage guarantees.

## Usage

``` r
conformal_cv(
  x,
  y,
  model,
  x_new = NULL,
  alpha = 0.1,
  n_folds = 10,
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
  intervals are computed for the training data using leave-one-fold-out
  predictions. Note: when `x_new = NULL`, prediction intervals for
  training observations use a self-consistent approximation. For exact
  CV+ intervals on new data, provide `x_new`.

- alpha:

  Miscoverage level. Default `0.10` gives 90 percent prediction
  intervals.

- n_folds:

  Number of cross-validation folds. Default `10`.

- verbose:

  Logical. If `TRUE`, shows a progress bar during fold fitting. Default
  `FALSE`.

- seed:

  Optional random seed for reproducible data splitting.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"cv_plus"`. The object also
stores `fold_models` (list of K fitted models), `fold_ids` (integer
vector mapping each observation to its fold), and `residuals`
(leave-fold-out absolute residuals), which are needed by the
[`predict()`](https://rdrr.io/r/stats/predict.html) method to compute
CV+ intervals for new data.

## Details

Unlike basic CV conformal prediction (which computes a single quantile
of CV residuals), CV+ constructs intervals that vary per test point. For
each test point, every training observation contributes a lower and
upper value based on the fold model that excluded that observation,
evaluated at the test point, plus or minus the leave-fold-out residual
for that observation. The interval bounds are then taken as quantiles of
these per-observation values.

The CV+ theoretical coverage guarantee is \\1 - 2\alpha\\, not \\1 -
\alpha\\ (Barber et al. 2021, Theorem 2). This is weaker than split
conformal's \\1 - \alpha\\ guarantee. In practice, CV+ coverage is
typically much closer to \\1 - \alpha\\.

## References

Barber, R.F., Candes, E.J., Ramdas, A. and Tibshirani, R.J. (2021).
Predictive inference with the Jackknife+. *Annals of Statistics*, 49(1),
486-507. [doi:10.1214/20-AOS1965](https://doi.org/10.1214/20-AOS1965)

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
[`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md),
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md),
[`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)

## Examples

``` r
set.seed(42)
n <- 200
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(20 * 3), ncol = 3)

# \donttest{
result <- conformal_cv(x, y, model = y ~ ., x_new = x_new, n_folds = 5)
print(result)
#> 
#> ── Conformal Prediction Intervals (CV+) ────────────────────────────────────────
#> • Coverage target: "90%"
#> • Training: 200 | Calibration: 200 | Predictions: 20
#> • Median residual: 0.6838
#> • Median interval width: 3.175
# }
```
