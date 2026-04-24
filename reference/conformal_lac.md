# Least Ambiguous Classifier Prediction Sets

Constructs prediction sets using the Least Ambiguous Classifier (LAC)
method. Includes all classes whose predicted probability exceeds
`1 - q`, where `q` is the conformal quantile of `1 - p(true class)`
scores.

## Usage

``` r
conformal_lac(x, y, model, x_new, alpha = 0.1, cal_fraction = 0.5, seed = NULL)
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

- seed:

  Optional random seed.

## Value

A `predictset_class` object with components:

- sets:

  A list of character vectors, one per new observation.

- probs:

  A list of named numeric vectors with predicted probabilities for
  included classes.

- alpha:

  The miscoverage level used.

- method:

  Character string `"lac"`.

- scores:

  Numeric vector of calibration scores.

- quantile:

  The conformal quantile used.

- classes:

  Character vector of all class labels.

- n_cal:

  Number of calibration observations.

- n_train:

  Number of training observations.

- fitted_model:

  The fitted model object.

- model:

  The `predictset_model` specification.

## References

Sadinle, M., Lei, J. and Wasserman, L. (2019). Least ambiguous
set-valued classifiers with bounded error levels. *Journal of the
American Statistical Association*, 114(525), 223-234.
[doi:10.1080/01621459.2017.1395341](https://doi.org/10.1080/01621459.2017.1395341)

## See also

Other classification methods:
[`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md),
[`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md),
[`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md)

## Examples

``` r
set.seed(42)
n <- 300
x <- matrix(rnorm(n * 4), ncol = 4)
y <- factor(ifelse(x[,1] + x[,2] > 0, "A", "B"))
x_new <- matrix(rnorm(50 * 4), ncol = 4)

clf <- make_model(
  train_fun = function(x, y) glm(y ~ ., data = data.frame(y = y, x),
                                  family = "binomial"),
  predict_fun = function(object, x_new) {
    df <- as.data.frame(x_new)
    names(df) <- paste0("X", seq_len(ncol(x_new)))
    p <- predict(object, newdata = df, type = "response")
    cbind(A = 1 - p, B = p)
  },
  type = "classification"
)

result <- conformal_lac(x, y, model = clf, x_new = x_new)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
print(result)
#> 
#> ── Conformal Prediction Sets (Least Ambiguous Classifier) ──────────────────────
#> • Coverage target: "90%"
#> • Classes: "A, B"
#> • Training: 150 | Calibration: 150 | Predictions: 50
#> • Median set size: 1 | Mean set size: 1
```
