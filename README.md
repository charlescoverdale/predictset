# predictset
[![CRAN status](https://www.r-pkg.org/badges/version/predictset)](https://CRAN.R-project.org/package=predictset)
[![Downloads](https://cranlogs.r-pkg.org/badges/predictset)](https://CRAN.R-project.org/package=predictset)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**predictset** is an R package for model-agnostic conformal prediction and distribution-free uncertainty quantification. It constructs prediction intervals (regression) and prediction sets (classification) with finite-sample coverage guarantees — no distributional assumptions required. Works with any model: `lm`, `glm`, `ranger`, `xgboost`, or custom user-defined models via `make_model()`.

## What is conformal prediction?

Standard machine learning models produce point predictions: a single number for regression, a single class for classification. But in practice, you almost always need to know how uncertain that prediction is. Conformal prediction is a framework for wrapping any model in a layer of calibrated uncertainty quantification. Given a target coverage level (say 90%), it produces prediction intervals or prediction sets that are guaranteed to contain the true value at least 90% of the time, regardless of the underlying model or data distribution.

The key property is that this guarantee holds in finite samples. It's not asymptotic, and it doesn't require distributional assumptions. The only requirement is that the calibration data and test data are exchangeable (roughly: drawn from the same distribution). This makes conformal prediction fundamentally different from parametric confidence intervals, bootstrap intervals, or Bayesian credible intervals, all of which depend on modelling assumptions that may not hold.

**predictset** implements the main conformal methods from the recent literature (split conformal, Jackknife+, CV+, conformalized quantile regression for regression, and split conformal, APS, RAPS, and LAC for classification) in a lightweight package with only two dependencies (`cli` and `stats`).

---

## How does predictset compare to other packages?

| Feature | **predictset** | **probably** | **conformalInference** | **MAPIE** |
|---|---|---|---|---|
| Language | R | R | R | Python |
| Regression | Yes | Yes | Yes | Yes |
| Classification | Yes | No | No | Yes |
| Model-agnostic | Yes | tidymodels only | Yes | scikit-learn only |
| On CRAN | Pending | Yes | No (GitHub only) | N/A |
| Jackknife+ / CV+ | Yes | No | Yes | Yes |
| CQR | Yes | Yes | Yes | Yes |
| APS / RAPS | Yes | No | No | Yes |
| Mondrian CP | Yes | No | No | Yes |
| Weighted CP | Yes | No | No | Yes |
| Adaptive CI | Yes | No | No | No |
| Conditional diagnostics | Yes | No | No | Partial |
| Dependencies | 2 | 14+ | 5 | N/A |
| Last updated | 2026 | 2024 | 2019 | 2024 |

**predictset** is designed to complement rather than compete with `probably`. If you're working in the tidymodels ecosystem and only need regression intervals, `probably` integrates neatly with your workflow. **predictset** fills the gaps: classification methods (APS, RAPS, LAC), Jackknife+/CV+ for regression, and a model-agnostic interface that works with any model, not just tidymodels workflows.

`conformalInference` by Ryan Tibshirani was foundational research code, but it hasn't been updated since 2019, isn't on CRAN, and doesn't cover classification.

---

## Installation

```r
# Install from CRAN
install.packages("predictset")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("charlescoverdale/predictset")
```

---

## Methods

| Function | Type | Method | Reference |
|---|---|---|---|
| `conformal_split()` | Regression | Split conformal | [Vovk et al. (2005)](https://link.springer.com/book/10.1007/978-3-031-06649-8) |
| `conformal_cv()` | Regression | CV+ | [Barber et al. (2021)](https://doi.org/10.1214/20-AOS1965) |
| `conformal_jackknife()` | Regression | Jackknife+ | [Barber et al. (2021)](https://doi.org/10.1214/20-AOS1965) |
| `conformal_cqr()` | Regression | Conformalized Quantile Regression | [Romano et al. (2019)](https://arxiv.org/abs/1905.03222) |
| `conformal_mondrian()` | Regression | Mondrian (group-conditional) | [Vovk et al. (2005)](https://link.springer.com/book/10.1007/978-3-031-06649-8) |
| `conformal_weighted()` | Regression | Weighted conformal (covariate shift) | [Tibshirani et al. (2019)](https://arxiv.org/abs/1904.06019) |
| `conformal_aps()` | Classification | Adaptive Prediction Sets | [Romano, Sesia & Candes (2020)](https://arxiv.org/abs/2006.02544) |
| `conformal_raps()` | Classification | Regularized APS | [Angelopoulos et al. (2021)](https://arxiv.org/abs/2009.14193) |
| `conformal_lac()` | Classification | Least Ambiguous Classifier | [Sadinle, Lei & Wasserman (2019)](https://doi.org/10.1080/01621459.2018.1449837) |
| `conformal_mondrian_class()` | Classification | Mondrian (group-conditional) | [Vovk et al. (2005)](https://link.springer.com/book/10.1007/978-3-031-06649-8) |
| `conformal_aci()` | Sequential | Adaptive Conformal Inference | [Gibbs & Candes (2021)](https://arxiv.org/abs/2106.00170) |
| `coverage()` | Diagnostic | Empirical coverage rate | |
| `coverage_by_group()` | Diagnostic | Coverage within subgroups | |
| `coverage_by_bin()` | Diagnostic | Coverage by prediction quantile bin | |
| `interval_width()` | Diagnostic | Width of prediction intervals | |
| `set_size()` | Diagnostic | Size of prediction sets | |
| `conformal_pvalue()` | Diagnostic | Conformal p-values | |
| `conformal_compare()` | Diagnostic | Compare multiple methods | |
| `make_model()` | Utility | Wrap custom train/predict functions | |

---

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
#> ── Conformal Prediction Intervals (Split Conformal) ──
#> • Coverage target: "90%"
#> • Training: 250 | Calibration: 250 | Predictions: 100
#> • Conformal quantile: 1.876
#> • Median interval width: 3.7519
```

---

## Model interface

There are three ways to specify a model. This flexibility means **predictset** works with anything from a simple linear model to a custom deep learning wrapper.

**1. Formula shorthand** (fits `lm` internally):

```r
result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
```

This is the quickest way to get started. Pass a formula and **predictset** handles the fitting.

**2. Fitted model** (auto-detected for `lm`, `glm`, and `ranger`):

```r
fit <- lm(y ~ ., data = data.frame(y = y, x))
result <- conformal_split(x, y, model = fit, x_new = x_new)
```

If you've already fitted a model and want conformal intervals around its predictions, pass it directly. **predictset** recognises standard R model objects and extracts the training and prediction functions automatically.

**3. Custom model** via `make_model()` (works with anything):

```r
xgb_model <- make_model(
  train_fun = function(x, y) {
    dtrain <- xgboost::xgb.DMatrix(x, label = y)
    xgboost::xgb.train(list(objective = "reg:squarederror"), dtrain, nrounds = 100)
  },
  predict_fun = function(object, x_new) {
    predict(object, xgboost::xgb.DMatrix(x_new))
  },
  type = "regression"
)

result <- conformal_split(x, y, model = xgb_model, x_new = x_new)
```

`make_model()` takes a training function, a prediction function, and a type (`"regression"` or `"classification"`). This is how you use conformal prediction with xgboost, keras, lightgbm, or any other model.

---

## Examples

### Classification with Adaptive Prediction Sets

APS produces prediction sets that adapt to the difficulty of each observation. Easy cases get small sets (often a single class), while ambiguous cases get larger sets. This is the recommended default for multi-class classification.

```r
library(predictset)

# Simulate 3-class classification
set.seed(42)
n <- 600
x <- matrix(rnorm(n * 4), ncol = 4)
y <- factor(ifelse(x[, 1] > 0.5, "A", ifelse(x[, 2] > 0, "B", "C")))
x_new <- matrix(rnorm(100 * 4), ncol = 4)

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

# Most predictions are a single class; ambiguous ones include 2-3
table(set_size(result))
```

### Jackknife+ with ranger

Jackknife+ uses leave-one-out refitting to produce prediction intervals without splitting the data. This gives tighter intervals than split conformal (because it uses all the data for both training and calibration) at the cost of refitting the model n times.

```r
set.seed(42)
n <- 200
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1]^2 + x[, 2] + rnorm(n, sd = 0.5)
x_new <- matrix(rnorm(50 * 3), ncol = 3)

rf <- make_model(
  train_fun = function(x, y) {
    ranger::ranger(y ~ ., data = data.frame(y = y, x))
  },
  predict_fun = function(object, x_new) {
    predict(object, data = as.data.frame(x_new))$predictions
  },
  type = "regression"
)

result <- conformal_jackknife(x, y, model = rf, x_new = x_new, alpha = 0.10)
print(result)
plot(result)
```

### Normalised conformal for heteroscedastic data

When the noise varies across the input space (e.g. predictions are more uncertain at extreme values), standard conformal intervals are too wide in low-noise regions and too narrow in high-noise ones. Normalised conformal scoring fixes this by scaling residuals by a local estimate of variability.

```r
set.seed(42)
n <- 500
x <- matrix(runif(n, 0, 10), ncol = 1)
y <- sin(x[, 1]) + rnorm(n, sd = 0.1 + 0.3 * x[, 1])  # noise grows with x
x_new <- matrix(seq(0, 10, length.out = 100), ncol = 1)

result <- conformal_split(
  x, y, model = y ~ ., x_new = x_new, alpha = 0.10,
  score_type = "normalized"
)

# Intervals are narrower near x = 0, wider near x = 10
plot(result)
```

### Mondrian conformal: group-conditional coverage

Standard conformal prediction guarantees marginal coverage (across all test points), but coverage can vary wildly across subgroups. Mondrian conformal computes a separate quantile for each group, guaranteeing coverage within each subgroup. This is critical for fairness and regulatory compliance.

No other R package on CRAN implements Mondrian conformal prediction.

```r
set.seed(42)
n <- 600
x <- matrix(rnorm(n * 3), ncol = 3)
groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
y <- x[, 1] * 2 + ifelse(groups == "high", 3, 0.5) * rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
                              groups = groups, groups_new = groups_new)
print(result)

# Check per-group coverage
y_new <- x_new[, 1] * 2 + ifelse(groups_new == "high", 3, 0.5) * rnorm(200)
coverage_by_group(result, y_new, groups_new)
```

### Weighted conformal: handling covariate shift

When the test distribution differs from the training distribution (covariate shift), standard conformal coverage guarantees break down. Weighted conformal prediction uses importance weights to correct for this shift.

```r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(100 * 3, mean = 1), ncol = 3)  # shifted test data

# Importance weights (likelihood ratio of test vs training distributions)
weights <- dnorm(x[, 1], mean = 1) / dnorm(x[, 1], mean = 0)

result <- conformal_weighted(x, y, model = y ~ ., x_new = x_new,
                              weights = weights)
print(result)
```

### Comparing methods

Benchmark multiple conformal methods side-by-side:

```r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(200)

comp <- conformal_compare(x, y, model = y ~ ., x_new = x_new, y_new = y_new,
                           methods = c("split", "cv", "jackknife"))
print(comp)
```

### Conformalized Quantile Regression

CQR combines conformal prediction with quantile regression to produce intervals that naturally adapt to heteroscedasticity. Instead of fitting a model for the mean and adding symmetric bands, CQR fits models for the lower and upper quantiles and then adjusts them to guarantee coverage.

```r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] + x[, 2]^2 + rnorm(n, sd = 0.5 + abs(x[, 1]))
x_new <- matrix(rnorm(100 * 3), ncol = 3)

# In practice, use quantile regression (e.g. quantreg::rq).
# Here we approximate with shifted linear models for illustration.
model_lo <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(obj, x_new) predict(obj, newdata = as.data.frame(x_new)) - 1.5,
  type = "regression"
)
model_hi <- make_model(
  train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
  predict_fun = function(obj, x_new) predict(obj, newdata = as.data.frame(x_new)) + 1.5,
  type = "regression"
)

result <- conformal_cqr(x, y, model_lo, model_hi, x_new = x_new, alpha = 0.10)
print(result)
plot(result)
```

### Adaptive Conformal Inference (sequential prediction)

ACI adapts the miscoverage level online based on observed coverage, maintaining long-run coverage even under distribution shift. No other R package implements ACI.

```r
set.seed(42)
n <- 500
y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)  # drifting process
y_pred <- c(0, y_true[-n])                         # naive lag-1 predictor

result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)
print(result)
plot(result)  # intervals + adaptive alpha trace
```

---

## Diagnostics

After producing predictions, use the diagnostic functions to evaluate calibration and efficiency.

```r
# Suppose y_test contains the true values for x_new
coverage(result, y_test)
#> [1] 0.91

# Average interval width (regression)
mean(interval_width(result))
#> [1] 3.42

# Prediction set sizes (classification)
table(set_size(result))
#>  1  2  3
#> 74 21  5

# Coverage within subgroups
coverage_by_group(result, y_test, groups = groups_test)
#>   group coverage   n target
#> 1  high    0.920  98    0.9
#> 2   low    0.891 102    0.9

# Coverage by prediction quantile bin
coverage_by_bin(result, y_test, bins = 5)

# Conformal p-values for outlier detection
pvals <- conformal_pvalue(result$scores, new_scores)
```

`coverage()` should be close to `1 - alpha`. If it's substantially lower, something has gone wrong (likely a violation of exchangeability). `interval_width()` and `set_size()` measure efficiency: narrower intervals and smaller sets are better, conditional on achieving the target coverage.

`coverage_by_group()` and `coverage_by_bin()` diagnose conditional coverage. Marginal coverage can mask severe under-coverage in subgroups (e.g. by demographic group or by prediction magnitude). These diagnostics help identify where intervals fail, and are essential for fairness evaluation.

---

## Theory and references

Conformal prediction was introduced by Vovk, Gammerman, and Shafer in the early 2000s. The key insight is that if calibration and test data are exchangeable (i.e. their joint distribution is invariant to permutation), then the conformal p-value is uniformly distributed, which gives an exact finite-sample coverage guarantee. Unlike bootstrap or Bayesian intervals, this guarantee holds regardless of model misspecification.

The recent explosion of interest in conformal prediction has been driven by several methodological advances that make it practical for modern machine learning:

- **[Vovk](https://scholar.google.com/citations?user=OcfGKs0AAAAJ), [Gammerman](https://scholar.google.com/citations?user=0iJKGYoAAAAJ), [Shafer](https://scholar.google.com/citations?user=LdG9r8EAAAAJ) (2005)**. [*Algorithmic Learning in a Random World*](https://link.springer.com/book/10.1007/978-3-031-06649-8). Springer. The foundational book introducing conformal prediction.
- **[Lei](https://scholar.google.com/citations?user=tJRRdEIAAAAJ), G'Sell, Rinaldo, [Tibshirani](https://scholar.google.com/citations?user=z3678L4AAAAJ), [Wasserman](https://scholar.google.com/citations?user=gFb81CEAAAAJ) (2018)**. [Distribution-free predictive inference for regression](https://doi.org/10.1080/01621459.2017.1307116). *Journal of the American Statistical Association*, 113(523), 1094–1111. Formalises split conformal prediction.
- **[Barber](https://scholar.google.com/citations?user=MKGlXm0AAAAJ), [Candes](https://scholar.google.com/citations?user=nRQi4O8AAAAJ), [Ramdas](https://scholar.google.com/citations?user=OQUI1uEAAAAJ), [Tibshirani](https://scholar.google.com/citations?user=z3678L4AAAAJ) (2021)**. [Predictive inference with the Jackknife+](https://doi.org/10.1214/20-AOS1965). *Annals of Statistics*, 49(1), 486–507. Introduces Jackknife+ and CV+, which avoid the efficiency loss from data splitting.
- **[Romano](https://scholar.google.com/citations?user=uxUBMqgAAAAJ), Patterson, [Candes](https://scholar.google.com/citations?user=nRQi4O8AAAAJ) (2019)**. [Conformalized quantile regression](https://arxiv.org/abs/1905.03222). *NeurIPS 2019*. Combines quantile regression with conformal calibration for adaptive intervals.
- **[Romano](https://scholar.google.com/citations?user=uxUBMqgAAAAJ), [Sesia](https://scholar.google.com/citations?user=5kKi8sQAAAAJ), [Candes](https://scholar.google.com/citations?user=nRQi4O8AAAAJ) (2020)**. [Classification with valid and adaptive coverage](https://arxiv.org/abs/2006.02544). *NeurIPS 2020*. Introduces Adaptive Prediction Sets (APS) for classification.
- **[Angelopoulos](https://scholar.google.com/citations?user=3GwMBBcAAAAJ), [Bates](https://scholar.google.com/citations?user=ZFT-FuAAAAAJ), Malik, [Jordan](https://scholar.google.com/citations?user=iKPWydkAAAAJ) (2021)**. [Uncertainty sets for image classifiers using conformal prediction](https://arxiv.org/abs/2009.14193). *ICLR 2021*. Introduces Regularized APS (RAPS) to reduce set sizes.
- **[Sadinle](https://scholar.google.com/citations?user=UU9G3mMAAAAJ), [Lei](https://scholar.google.com/citations?user=tJRRdEIAAAAJ), [Wasserman](https://scholar.google.com/citations?user=gFb81CEAAAAJ) (2019)**. [Least ambiguous set-valued classifiers with bounded error levels](https://doi.org/10.1080/01621459.2018.1449837). *Journal of the American Statistical Association*, 114(525), 223–234. The LAC method for classification.
- **[Tibshirani](https://scholar.google.com/citations?user=z3678L4AAAAJ), [Barber](https://scholar.google.com/citations?user=MKGlXm0AAAAJ), [Candes](https://scholar.google.com/citations?user=nRQi4O8AAAAJ), [Ramdas](https://scholar.google.com/citations?user=OQUI1uEAAAAJ) (2019)**. [Conformal prediction under covariate shift](https://arxiv.org/abs/1904.06019). *NeurIPS 2019*. Weighted conformal prediction for distribution shift.
- **[Gibbs](https://scholar.google.com/citations?user=DnorJJYAAAAJ), [Candes](https://scholar.google.com/citations?user=nRQi4O8AAAAJ) (2021)**. [Adaptive conformal inference under distribution shift](https://arxiv.org/abs/2106.00170). *NeurIPS 2021*. Online alpha adjustment for sequential prediction.

For an accessible introduction to the field, see [Angelopoulos](https://scholar.google.com/citations?user=3GwMBBcAAAAJ) and [Bates](https://scholar.google.com/citations?user=ZFT-FuAAAAAJ) (2023), [A Gentle Introduction to Conformal Prediction and Distribution-Free Uncertainty Quantification](https://arxiv.org/abs/2107.07511).

---

## Limitations

- **Split methods halve the training data.** Split conformal, APS, RAPS, and LAC all divide the data into a training set and a calibration set. With small datasets, this can noticeably reduce model quality. Jackknife+ and CV+ avoid this at the cost of refitting the model multiple times.
- **Jackknife+ and CV+ are computationally expensive.** Jackknife+ refits the model n times; CV+ refits it K times. For large datasets or expensive models, this may be impractical.
- **The coverage guarantee requires exchangeability.** If the calibration data and test data come from different distributions (for example, if there is temporal drift) the coverage guarantee does not hold. This means conformal prediction is not directly applicable to time series forecasting without modification (e.g. conformal methods for time series exist but are not implemented here).
- **Classification methods depend on probability estimates.** APS, RAPS, and LAC require the model to output well-calibrated class probabilities. If the probabilities are poorly calibrated, the prediction sets will still have valid coverage but may be unnecessarily large.

---

## Related packages

| Package | Description |
|---|---|
| [probably](https://probably.tidymodels.org/) | Conformal regression within the tidymodels ecosystem |
| [conformalInference](https://github.com/ryantibs/conformal) | Research code by Tibshirani et al. (2019, GitHub only) |
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data |
| [boe](https://github.com/charlescoverdale/boe) | Bank of England data |
| [fred](https://github.com/charlescoverdale/fred) | Federal Reserve Economic Data (FRED) |
| [readecb](https://github.com/charlescoverdale/readecb) | European Central Bank data |
| [readoecd](https://github.com/charlescoverdale/readoecd) | OECD data |
| [inflateR](https://github.com/charlescoverdale/inflateR) | Inflation adjustment |

---

## Issues

Found a bug or have a feature request? Please [open an issue](https://github.com/charlescoverdale/predictset/issues) on GitHub.

---

## Keywords

conformal prediction, prediction intervals, prediction sets, uncertainty quantification, coverage guarantee, distribution-free inference, model-agnostic, split conformal, jackknife+, CV+, conformalized quantile regression, adaptive prediction sets, RAPS, Mondrian conformal, weighted conformal, covariate shift, fairness, conditional coverage, calibration, R package
