# Split Conformal Prediction Sets for Classification

**\[Deprecated\]** `conformal_class_split()` is identical to
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md)
and is deprecated. Use
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md)
instead.

## Usage

``` r
conformal_class_split(
  x,
  y,
  model,
  x_new,
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

A `predictset_class` object. See
[`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md)
for details.

## References

Sadinle, M., Lei, J. and Wasserman, L. (2019). Least ambiguous
set-valued classifiers with bounded error levels. *Journal of the
American Statistical Association*, 114(525), 223-234.
[doi:10.1080/01621459.2017.1395341](https://doi.org/10.1080/01621459.2017.1395341)

## Examples

``` r
set.seed(42)
n <- 300
x <- matrix(rnorm(n * 4), ncol = 4)
y <- factor(ifelse(x[,1] + x[,2] > 0, "A", "B"))
x_new <- matrix(rnorm(50 * 4), ncol = 4)

clf <- make_model(
  train_fun = function(x, y) {
    df <- data.frame(y = y, x)
    glm(y ~ ., data = df, family = "binomial")
  },
  predict_fun = function(object, x_new) {
    df <- as.data.frame(x_new)
    names(df) <- paste0("X", seq_len(ncol(x_new)))
    p <- predict(object, newdata = df, type = "response")
    cbind(A = 1 - p, B = p)
  },
  type = "classification"
)

# \donttest{
suppressWarnings(
  result <- conformal_class_split(x, y, model = clf, x_new = x_new)
)
# }
```
