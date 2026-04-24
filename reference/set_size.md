# Prediction Set Sizes

Returns the number of classes in each prediction set.

## Usage

``` r
set_size(object)
```

## Arguments

- object:

  A `predictset_class` object.

## Value

An integer vector of set sizes.

## See also

Other diagnostics:
[`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md),
[`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md),
[`coverage()`](https://charlescoverdale.github.io/predictset/reference/coverage.md),
[`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md),
[`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md),
[`interval_width()`](https://charlescoverdale.github.io/predictset/reference/interval_width.md)

## Examples

``` r
set.seed(42)
n <- 300
x <- matrix(rnorm(n * 4), ncol = 4)
y <- factor(ifelse(x[,1] > 0, "A", "B"))
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
sizes <- set_size(result)
table(sizes)
#> sizes
#>  1 
#> 50 
```
