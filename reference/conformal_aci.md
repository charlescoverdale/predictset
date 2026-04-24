# Adaptive Conformal Inference

Implements basic Adaptive Conformal Inference (ACI) for sequential
prediction. The miscoverage level alpha is adjusted online based on
whether previous predictions covered the true values, maintaining
long-run coverage even under distribution shift.

## Usage

``` r
conformal_aci(y_pred, y_true, alpha = 0.1, gamma = 0.005)
```

## Arguments

- y_pred:

  A numeric vector of point predictions (sequential).

- y_true:

  A numeric vector of true values (sequential).

- alpha:

  Target miscoverage level. Default `0.10`.

- gamma:

  Learning rate for alpha adjustment. Default `0.005`. Larger values
  adapt faster but are less stable.

## Value

A list with components:

- lower:

  Numeric vector of lower bounds.

- upper:

  Numeric vector of upper bounds.

- covered:

  Logical vector indicating whether each interval covered the true
  value.

- alphas:

  Numeric vector of the adapted alpha values at each step.

- coverage:

  Overall empirical coverage.

## Details

ACI provides asymptotic coverage guarantees under distribution drift,
not the finite-sample guarantees of split conformal prediction. The
long-run average coverage converges to \\1 - \alpha\\ as the sequence
length grows (Gibbs and Candes, 2021).

## References

Gibbs, I. and Candes, E. (2021). Adaptive conformal inference under
distribution shift. *Advances in Neural Information Processing Systems*,
34.

## See also

Other regression methods:
[`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md),
[`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
[`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
[`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md),
[`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md),
[`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)

## Examples

``` r
set.seed(42)
n <- 200
y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)
y_pred <- c(0, y_true[-n])  # naive lag-1 prediction

result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)
print(result$coverage)
#> [1] 0.99
```
