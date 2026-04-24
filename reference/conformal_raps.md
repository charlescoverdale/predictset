# Regularized Adaptive Prediction Sets

Constructs prediction sets using the Regularized Adaptive Prediction
Sets (RAPS) method of Angelopoulos et al. (2021). Extends APS with a
regularization penalty that encourages smaller prediction sets.

## Usage

``` r
conformal_raps(
  x,
  y,
  model,
  x_new,
  alpha = 0.1,
  cal_fraction = 0.5,
  k_reg = 1,
  lambda = 0.01,
  randomize = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictor variables.

- y:

  A factor (or character/integer vector coerced to factor) of class
  labels.

- model:

  A
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification with `type = "classification"`, or a fitted model object
  that produces class probabilities.

- x_new:

  A numeric matrix or data frame of new predictor variables.

- alpha:

  Miscoverage level. Default `0.10` gives 90 percent prediction sets.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- k_reg:

  Regularization parameter controlling the number of classes exempt from
  the penalty. Default `1` (only the top class is unpenalized).

- lambda:

  Regularization strength. Default `0.01`. Larger values produce smaller
  prediction sets at the potential cost of coverage.

- randomize:

  Logical. If `TRUE`, uses randomized scores for exact coverage (but
  prediction sets become stochastic). Default `FALSE`.

- seed:

  Optional random seed.

## Value

A `predictset_class` object. See
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md)
for details. The `method` component is `"raps"`.

## References

Angelopoulos, A.N., Bates, S., Malik, J. and Jordan, M.I. (2021).
Uncertainty sets for image classifiers using conformal prediction.
*International Conference on Learning Representations*.
[doi:10.48550/arXiv.2009.14193](https://doi.org/10.48550/arXiv.2009.14193)

## See also

Other classification methods:
[`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md),
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md),
[`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md)

## Examples

``` r
set.seed(42)
n <- 300
x <- matrix(rnorm(n * 4), ncol = 4)
y <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
x_new <- matrix(rnorm(50 * 4), ncol = 4)

clf <- make_model(
  train_fun = function(x, y) glm(y ~ ., data = data.frame(y = y, x),
                                  family = "binomial"),
  predict_fun = function(object, x_new) {
    df <- as.data.frame(x_new)
    names(df) <- paste0("X", seq_len(ncol(x_new)))
    p <- predict(object, newdata = df, type = "response")
    cbind(A = p / 2, B = p / 2, C = 1 - p)
  },
  type = "classification"
)

# \donttest{
result <- conformal_raps(x, y, model = clf, x_new = x_new,
                          k_reg = 1, lambda = 0.01)
print(result)
#> 
#> ── Conformal Prediction Sets (Regularized Adaptive Prediction Sets) ────────────
#> • Coverage target: "90%"
#> • Classes: "A, B, C"
#> • Training: 150 | Calibration: 150 | Predictions: 50
#> • Median set size: 3 | Mean set size: 3
# }
```
