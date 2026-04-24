# Conformalized Quantile Regression

Constructs prediction intervals using Conformalized Quantile Regression
(Romano et al. 2019). Requires two models: one for the lower quantile
and one for the upper quantile. The conformal step adjusts these
quantile predictions to achieve valid coverage.

## Usage

``` r
conformal_cqr(
  x,
  y,
  model_lower,
  model_upper,
  x_new,
  alpha = 0.1,
  cal_fraction = 0.5,
  quantiles = c(0.05, 0.95),
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictor variables.

- y:

  A numeric vector of response values.

- model_lower:

  A
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification for the lower quantile model.

- model_upper:

  A
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification for the upper quantile model.

- x_new:

  A numeric matrix or data frame of new predictor variables.

- alpha:

  Miscoverage level. Default `0.10`.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- quantiles:

  The target quantiles. Default `c(0.05, 0.95)`.

- seed:

  Optional random seed.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"cqr"`.

## Details

Interval quality depends on the underlying quantile models. Poorly
calibrated quantile models produce valid but potentially wide intervals.
For best results, use proper quantile regression models (e.g.
`quantreg::rq()`) rather than shifted mean predictions.

## References

Romano, Y., Patterson, E. and Candes, E.J. (2019). Conformalized
quantile regression. *Advances in Neural Information Processing
Systems*, 32.
[doi:10.48550/arXiv.1905.03222](https://doi.org/10.48550/arXiv.1905.03222)

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
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

# Approximating quantile regression with shifted linear models.
# In practice, use quantile regression models, e.g.:
#   quantreg::rq(y ~ ., data = df, tau = 0.05)
model_lo <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(obj, x_new) {
    predict(obj, newdata = as.data.frame(x_new)) - 1.5
  },
  type = "regression"
)
model_hi <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(obj, x_new) {
    predict(obj, newdata = as.data.frame(x_new)) + 1.5
  },
  type = "regression"
)

result <- conformal_cqr(x, y, model_lo, model_hi, x_new = x_new)
print(result)
#> 
#> ── Conformal Prediction Intervals (Conformalized Quantile Regression) ──────────
#> • Coverage target: "90%"
#> • Training: 100 | Calibration: 100 | Predictions: 20
#> • Conformal quantile: -0.038
#> • Median interval width: 2.924
```
