# Weighted Conformal Prediction Intervals

Constructs prediction intervals using weighted split conformal
inference, designed for settings with covariate shift where calibration
and test data may have different distributions. Importance weights
re-weight the calibration scores to account for this shift.

## Usage

``` r
conformal_weighted(
  x,
  y,
  model,
  x_new,
  weights = NULL,
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

- weights:

  A numeric vector of importance weights for each observation in `x`,
  with length equal to `nrow(x)`. Weights must be non-negative. If
  `NULL`, uniform weights are used (equivalent to standard split
  conformal).

- alpha:

  Miscoverage level. Default `0.10`.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- seed:

  Optional random seed.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"weighted"`.

## Details

The test-point weight \\w\_{n+1}\\ is set to the mean of calibration
weights, following standard practice when the true test weight is
unknown. See Tibshirani et al. (2019), Equation 5.

## References

Tibshirani, R.J., Barber, R.F., Candes, E.J. and Ramdas, A. (2019).
Conformal prediction under covariate shift. *Advances in Neural
Information Processing Systems*, 32.

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
[`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
[`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md),
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)

## Examples

``` r
set.seed(42)
n <- 400
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(50 * 3, mean = 1), ncol = 3)
weights <- rep(1, n)

# \donttest{
result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
                              weights = weights)
print(result)
#> 
#> ── Conformal Prediction Intervals (Weighted Conformal) ─────────────────────────
#> • Coverage target: "90%"
#> • Training: 200 | Calibration: 200 | Predictions: 50
#> • Conformal quantile: 1.648
#> • Median interval width: 3.296
# }
```
