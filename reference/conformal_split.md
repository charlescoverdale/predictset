# Split Conformal Prediction Intervals

Constructs prediction intervals using split conformal inference. The
data is split into training and calibration sets; nonconformity scores
are computed on the calibration set and used to form intervals on new
data.

## Usage

``` r
conformal_split(
  x,
  y,
  model,
  x_new,
  alpha = 0.1,
  cal_fraction = 0.5,
  score_type = c("absolute", "normalized"),
  scale_model = NULL,
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

  A numeric matrix or data frame of new predictor variables for which to
  compute prediction intervals.

- alpha:

  Miscoverage level. Default `0.10` gives 90 percent prediction
  intervals.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- score_type:

  Type of nonconformity score. `"absolute"` (default) uses absolute
  residuals and produces constant-width intervals. `"normalized"`
  divides residuals by a local scale estimate from `scale_model`,
  producing locally-adaptive interval widths.

- scale_model:

  A
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification for predicting absolute residuals (used only when
  `score_type = "normalized"`). Must return positive predictions. If
  `NULL` and `score_type = "normalized"`, a default model is fitted
  using [`lm()`](https://rdrr.io/r/stats/lm.html) on absolute residuals.

- seed:

  Optional random seed for reproducible data splitting.

## Value

A `predictset_reg` object (a list) with components:

- pred:

  Numeric vector of point predictions for `x_new`.

- lower:

  Numeric vector of lower bounds.

- upper:

  Numeric vector of upper bounds.

- alpha:

  The miscoverage level used.

- method:

  Character string `"split"`.

- scores:

  Numeric vector of calibration nonconformity scores.

- quantile:

  The conformal quantile used to form intervals.

- n_cal:

  Number of calibration observations.

- n_train:

  Number of training observations.

- fitted_model:

  The fitted model object.

- model:

  The `predictset_model` specification.

## References

Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R.J. and Wasserman, L.
(2018). Distribution-free predictive inference for regression. *Journal
of the American Statistical Association*, 113(523), 1094-1111.
[doi:10.1080/01621459.2017.1307116](https://doi.org/10.1080/01621459.2017.1307116)

## See also

Other regression methods:
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
[`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
[`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md),
[`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)

## Examples

``` r
set.seed(42)
n <- 200
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(50 * 3), ncol = 3)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
print(result)
#> 
#> ── Conformal Prediction Intervals (Split Conformal) ────────────────────────────
#> • Coverage target: "90%"
#> • Training: 100 | Calibration: 100 | Predictions: 50
#> • Conformal quantile: 1.4914
#> • Median interval width: 2.9828
```
