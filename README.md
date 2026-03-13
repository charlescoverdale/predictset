# predictset

<!-- badges: start -->
<!-- badges: end -->

Model-agnostic conformal prediction for R. Constructs prediction intervals (regression) and prediction sets (classification) with finite-sample coverage guarantees.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("charlescoverdale/predictset")
```

## Methods

### Regression

| Function | Method | Reference |
|---|---|---|
| `conformal_split()` | Split conformal | Vovk et al. (2005) |
| `conformal_cv()` | CV+ | Barber et al. (2021) |
| `conformal_jackknife()` | Jackknife+ | Barber et al. (2021) |
| `conformal_cqr()` | Conformalized Quantile Regression | Romano et al. (2019) |

### Classification

| Function | Method | Reference |
|---|---|---|
| `conformal_class_split()` | Split conformal | Vovk et al. (2005) |
| `conformal_aps()` | Adaptive Prediction Sets | Romano, Sesia & Candes (2020) |
| `conformal_raps()` | Regularized APS | Angelopoulos et al. (2021) |
| `conformal_lac()` | Least Ambiguous Classifier | Sadinle, Lei & Wasserman (2019) |

## Quick start

### Regression: split conformal with lm

```r
library(predictset)

set.seed(42)
n <- 500
x <- matrix(rnorm(n * 5), ncol = 5)
y <- x[, 1] * 2 + x[, 2] + rnorm(n)
x_new <- matrix(rnorm(100 * 5), ncol = 5)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new, alpha = 0.10)
print(result)
plot(result)
```

### Classification: adaptive prediction sets

```r
clf <- make_model(
  train_fun = function(x, y) {
    ranger::ranger(y ~ ., data = data.frame(y = y, x), probability = TRUE)
  },
  predict_fun = function(object, x_new) {
    predict(object, data = as.data.frame(x_new))$predictions
  },
  type = "classification"
)

result <- conformal_aps(x, y, model = clf, x_new = x_new, alpha = 0.10)
print(result)
```

## Model interface

Three ways to specify a model:

**1. Formula shorthand** (fits `lm` internally):
```r
conformal_split(x, y, model = y ~ ., x_new = x_new)
```

**2. Fitted model** (auto-detected for `lm`, `glm`, `ranger`):
```r
fit <- lm(y ~ ., data = data.frame(y = y, x))
conformal_split(x, y, model = fit, x_new = x_new)
```

**3. Custom model** via `make_model()` (works with any model):
```r
my_model <- make_model(
  train_fun = function(x, y) { ... },
  predict_fun = function(object, x_new) { ... },
  type = "regression"
)
conformal_split(x, y, model = my_model, x_new = x_new)
```

## Diagnostics

```r
# Empirical coverage
coverage(result, y_true)

# Interval widths (regression)
interval_width(result)

# Set sizes (classification)
set_size(result)
```

## Related packages

- [probably](https://probably.tidymodels.org/) — conformal regression within tidymodels
- [conformalInference](https://github.com/ryantibs/conformal) — research code by Tibshirani et al.
