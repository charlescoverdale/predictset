# Mondrian Conformal Prediction Sets for Classification

Constructs prediction sets with group-conditional coverage guarantees
for classification. Uses LAC-style scoring with per-group conformal
quantiles.

## Usage

``` r
conformal_mondrian_class(
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

  A factor (or character/integer vector coerced to factor) of class
  labels.

- model:

  A
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  specification with `type = "classification"`, or a fitted model
  object.

- x_new:

  A numeric matrix or data frame of new predictor variables.

- groups:

  A factor or character vector of group labels for each observation in
  `x`.

- groups_new:

  A factor or character vector of group labels for each observation in
  `x_new`.

- alpha:

  Miscoverage level. Default `0.10`.

- cal_fraction:

  Fraction of data used for calibration. Default `0.5`.

- seed:

  Optional random seed.

## Value

A `predictset_class` object. See
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md)
for details. The `method` component is `"mondrian"`. Additional
components include `groups_new` and `group_quantiles`.

## See also

Other classification methods:
[`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md),
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md),
[`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md)

## Examples

``` r
set.seed(42)
n <- 400
x <- matrix(rnorm(n * 4), ncol = 4)
groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
y <- factor(ifelse(x[,1] + x[,2] > 0, "A", "B"))
x_new <- matrix(rnorm(50 * 4), ncol = 4)
groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

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

# \donttest{
result <- conformal_mondrian_class(x, y, model = clf, x_new = x_new,
                                    groups = groups, groups_new = groups_new)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
print(result)
#> 
#> ── Conformal Prediction Sets (Mondrian (Group-Conditional)) ────────────────────
#> • Coverage target: "90%"
#> • Classes: "A, B"
#> • Training: 200 | Calibration: 200 | Predictions: 50
#> • Median set size: 1 | Mean set size: 1
# }
```
