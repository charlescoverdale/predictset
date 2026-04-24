# Getting Started with predictset

## What is conformal prediction?

Machine learning models produce point predictions - a single number for
regression, a single class for classification. But in practice, you need
to know *how uncertain* that prediction is.

**Conformal prediction** wraps any model in a layer of calibrated
uncertainty quantification. Given a target coverage level (say 90%), it
produces:

- **Prediction intervals** (regression) guaranteed to contain the true
  value at least 90% of the time
- **Prediction sets** (classification) guaranteed to contain the true
  class at least 90% of the time

The key property: this guarantee holds in **finite samples**, without
distributional assumptions. The only requirement is that calibration and
test data are exchangeable (roughly: drawn from the same distribution).

`predictset` implements the main conformal methods from the recent
literature in a lightweight, model-agnostic R package with only two
dependencies (`cli` and `stats`).

## Quick start: regression

Split conformal prediction with a linear model using formula shorthand:

``` r
library(predictset)

set.seed(42)
n <- 500
x <- matrix(rnorm(n * 5), ncol = 5)
y <- x[, 1] * 2 + x[, 2] + rnorm(n)
x_new <- matrix(rnorm(100 * 5), ncol = 5)

result <- conformal_split(x, y, model = y ~ ., x_new = x_new, alpha = 0.10)
print(result)
#> 
#> ── Conformal Prediction Intervals (Split Conformal) ────────────────────────────
#> • Coverage target: "90%"
#> • Training: 250 | Calibration: 250 | Predictions: 100
#> • Conformal quantile: 1.876
#> • Median interval width: 3.7519
```

The `alpha` parameter controls the miscoverage rate: `alpha = 0.10`
targets 90% coverage.

## Model interface

There are three ways to specify a model:

**1. Formula shorthand** (fits `lm` internally):

``` r
result <- conformal_split(x, y, model = y ~ ., x_new = x_new)
```

**2. Fitted model** (auto-detected for `lm`, `glm`, `ranger`):

``` r
fit <- lm(y ~ ., data = data.frame(y = y, x))
result <- conformal_split(x, y, model = fit, x_new = x_new)
```

**3. Custom model** via
[`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
(works with anything):

``` r
rf <- make_model(
  train_fun = function(x, y) {
    ranger::ranger(y ~ ., data = data.frame(y = y, x))
  },
  predict_fun = function(object, x_new) {
    predict(object, data = as.data.frame(x_new))$predictions
  },
  type = "regression"
)

result <- conformal_split(x, y, model = rf, x_new = x_new)
```

## Classification with Adaptive Prediction Sets

APS produces prediction sets that adapt to the difficulty of each
observation. Easy cases get small sets (often a single class), while
ambiguous cases get larger sets.

``` r
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

## Mondrian conformal: group-conditional coverage

Standard conformal guarantees *marginal* coverage (across all test
points), but coverage can vary across subgroups. Mondrian conformal
computes a separate quantile for each group, guaranteeing coverage
*within* each subgroup. This is critical for fairness and regulatory
compliance.

``` r
set.seed(42)
n <- 600
x <- matrix(rnorm(n * 3), ncol = 3)
groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
y <- x[, 1] * 2 + ifelse(groups == "high", 3, 0.5) * rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))

result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
                              groups = groups, groups_new = groups_new)

# Check per-group coverage
y_new <- x_new[, 1] * 2 + ifelse(groups_new == "high", 3, 0.5) * rnorm(200)
coverage_by_group(result, y_new, groups_new)
#>   group  coverage   n target
#> 1  high 0.8672566 113    0.9
#> 2   low 0.8850575  87    0.9
```

## Diagnostics

After producing predictions, use diagnostic functions to evaluate
calibration and efficiency:

``` r
# Empirical coverage (should be close to 1 - alpha)
coverage(result, y_test)

# Average interval width (regression) - narrower is better
mean(interval_width(result))

# Prediction set sizes (classification) - smaller is better
table(set_size(result))

# Coverage within subgroups (fairness check)
coverage_by_group(result, y_test, groups = groups_test)

# Coverage by prediction quantile bin
coverage_by_bin(result, y_test, bins = 5)

# Conformal p-values for outlier detection
pvals <- conformal_pvalue(result$scores, new_scores)
```

## Comparing methods

Benchmark multiple conformal methods side-by-side with
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md):

``` r
set.seed(42)
n <- 500
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(200 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(200)

comp <- conformal_compare(x, y, model = y ~ ., x_new = x_new, y_new = y_new,
                           methods = c("split", "cv", "jackknife"))
print(comp)
#> 
#> ── Conformal Method Comparison ─────────────────────────────────────────────────
#> 
#> • split: coverage = 0.89, mean width = 3.388, time = 0.002s
#> • cv: coverage = 0.89, mean width = 3.308, time = 0.037s
#> • jackknife: coverage = 0.89, mean width = 3.335, time = 29.372s
```

## Choosing a method

| Scenario                            | Recommended method                                                                                                                                                                                                            | Why                                        |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|
| Large dataset, speed matters        | [`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md)                                                                                                                             | Single model fit, fastest                  |
| Small dataset, need tight intervals | [`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md) or [`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md)\*                    | Uses all data for training and calibration |
| Heteroscedastic data                | [`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md) or `conformal_split(..., score_type = "normalized")`                                                                            | Adaptive interval widths                   |
| Multi-class classification          | [`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md)                                                                                                                                 | Adaptive set sizes, well-calibrated        |
| Classification with many classes    | [`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md)                                                                                                                               | Regularized APS, smaller sets              |
| Coverage must hold per subgroup     | [`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md) / [`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md) | Group-conditional guarantees               |
| Covariate shift between train/test  | [`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)                                                                                                                       | Importance-weighted calibration            |
| Sequential/online prediction        | [`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md)\*\*                                                                                                                             | Adapts to distribution drift               |

\*Jackknife+ and CV+ have a theoretical coverage guarantee of 1-2α
(Barber et al. 2021), weaker than split conformal’s 1-α. In practice,
coverage is typically near 1-α. \*\*ACI provides asymptotic (not
finite-sample) coverage guarantees.

## Further reading

- Angelopoulos & Bates (2023). [A Gentle Introduction to Conformal
  Prediction and Distribution-Free Uncertainty
  Quantification](https://arxiv.org/abs/2107.07511). The best accessible
  introduction.
- Barber, Candes, Ramdas, Tibshirani (2021). [Predictive inference with
  the Jackknife+](https://doi.org/10.1214/20-AOS1965). Jackknife+ and
  CV+ methods.
- Romano, Sesia, Candes (2020). [Classification with valid and adaptive
  coverage](https://arxiv.org/abs/2006.02544). APS for classification.
