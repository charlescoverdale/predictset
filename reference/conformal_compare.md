# Compare Conformal Prediction Methods

Runs multiple conformal prediction methods on the same data and returns
a comparison data frame with coverage, interval width, and computation
time for each method.

## Usage

``` r
conformal_compare(
  x,
  y,
  model,
  x_new,
  y_new,
  methods = c("split", "cv"),
  alpha = 0.1,
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

- y_new:

  A numeric vector of true response values for `x_new`, used to compute
  empirical coverage.

- methods:

  Character vector of method names to compare. Default
  `c("split", "cv")`. Available methods: `"split"`, `"cv"`,
  `"jackknife"`, `"jackknife_basic"`.

- alpha:

  Miscoverage level. Default `0.10`.

- seed:

  Optional random seed.

## Value

A `predictset_compare` object (a data frame) with columns:

- method:

  Character. The method name.

- coverage:

  Numeric. Empirical coverage on `y_new`.

- mean_width:

  Numeric. Mean interval width.

- median_width:

  Numeric. Median interval width.

- time_seconds:

  Numeric. Elapsed time in seconds.

## See also

Other diagnostics:
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`interval_width()`](https://charlescoverdale.github.io/predictset/reference/interval_width.md),
[`set_size()`](https://charlescoverdale.github.io/predictset/reference/set_size.md)

## Examples

``` r
set.seed(42)
n <- 300
x <- matrix(rnorm(n * 3), ncol = 3)
y <- x[, 1] * 2 + rnorm(n)
x_new <- matrix(rnorm(100 * 3), ncol = 3)
y_new <- x_new[, 1] * 2 + rnorm(100)

# \donttest{
comp <- conformal_compare(x, y, model = y ~ ., x_new = x_new,
                           y_new = y_new)
print(comp)
#> 
#> ── Conformal Method Comparison ─────────────────────────────────────────────────
#> 
#> • split: coverage = 0.92, mean width = 3.292, time = 0.002s
#> • cv: coverage = 0.93, mean width = 3.446, time = 0.02s
# }
```
