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
  weights_new = NULL,
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

- weights_new:

  A numeric vector of importance weights for each observation in
  `x_new`, with length equal to `nrow(x_new)`. Supplying these gives the
  exact procedure of Tibshirani et al. (2019), in which each test point
  receives its own conformal quantile. If `NULL`, the mean calibration
  weight is substituted for every test point, which is an approximation
  (see Details).

- alpha:

  Miscoverage level. Default `0.10`.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- seed:

  Optional random seed. Set for the duration of the call only; the
  global random stream is restored on exit.

## Value

A `predictset_reg` object. See
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)
for details. The `method` component is `"weighted"`. The `quantile`
component is the median conformal quantile across test points;
`quantile_by_point` holds the full vector.

## Details

Tibshirani et al. (2019), Equation 5, defines the weighted conformal
quantile using the test-point weight \\w(X\_{n+1})\\, which is known at
test time. Each test point therefore receives a different quantile, and
that per-point adaptation is the mechanism by which the method corrects
for covariate shift. Supply `weights_new` to obtain it.

A test point whose weight is large relative to the calibration weights
receives an infinite quantile: the point mass at \\+\infty\\ carries
more than \\\alpha\\ of the weighted distribution, so no finite interval
is justified there. That is the correct answer, and it flags test
covariates the calibration set cannot support.

When `weights_new` is `NULL` the mean calibration weight is used for
every test point. This yields a single constant-width interval and does
not carry the finite-sample guarantee; it is offered only as a fallback
for when the likelihood ratio cannot be evaluated on the test
covariates.

## References

Tibshirani, R.J., Barber, R.F., Candes, E.J. and Ramdas, A. (2019).
Conformal prediction under covariate shift. *Advances in Neural
Information Processing Systems*, 32.

Barber, R.F., Candes, E.J., Ramdas, A. and Tibshirani, R.J. (2023).
Conformal prediction beyond exchangeability. *Annals of Statistics*,
51(2), 816-845.
[doi:10.1214/23-AOS2276](https://doi.org/10.1214/23-AOS2276)

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

# Likelihood ratio of the test density to the training density
w <- dnorm(x[, 1], mean = 1) / dnorm(x[, 1], mean = 0)
w_new <- dnorm(x_new[, 1], mean = 1) / dnorm(x_new[, 1], mean = 0)

# \donttest{
result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
                              weights = w, weights_new = w_new)
print(result)
#> 
#> ── Conformal Prediction Intervals (Weighted Conformal) ─────────────────────────
#> • Coverage target: "90%"
#> • Training: 200 | Calibration: 200 | Predictions: 50
#> • Conformal quantile: 1.5614 to 1.9626 across test points (median 1.5614)
#> • Median interval width: 3.1229
# }
```
