# Changelog

## predictset 0.4.0

This release corrects three defects that affected results, and changes
two defaults. Anyone using
[`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md),
[`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md),
[`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md),
or passing a formula or fitted model as `model` should re-run their
analysis.

### Bug fixes

- [`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md)
  applied the online update with the operands reversed:
  `alpha_t + gamma * (err_t - alpha)` instead of
  `alpha_t + gamma * (alpha - err_t)` (Gibbs and Candes 2021, Eq. 2). A
  miscoverage event therefore narrowed the next interval instead of
  widening it, turning the intended negative feedback into positive
  feedback, and `alpha_t` ran away to a clip boundary. Under a variance
  shift this drove empirical coverage to 0.605 against a 0.90 target.

- The `model` argument was discarded whenever it was a formula or a
  fitted model object, and a plain `lm(y ~ .)` was fitted in its place.
  `model = y ~ a` silently fitted every column of `x`;
  `lm(y ~ poly(v1, 3) + v2)` was refitted as `y ~ v1 + v2`; `ranger`
  hyperparameters were dropped. Formulas and fitted models are now
  honoured, refitted on each conformal split. Objects that cannot be
  refitted raise an error naming
  [`make_model()`](https://charlescoverdale.github.io/predictset/reference/make_model.md)
  instead of silently substituting a default.

- [`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md)
  and
  [`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md)
  could return the full label set for every observation. The set builder
  included the class that *crossed* the threshold rather than inverting
  the calibrated score, and `randomize = TRUE` randomised the
  calibration scores but never the set construction. With oracle
  probabilities on a four-class problem, APS returned a mean set size of
  3.90 out of 4 at 99.9% coverage; it now returns 2.69 at 88.6%.

- [`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md),
  [`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md),
  and [`predict()`](https://rdrr.io/r/stats/predict.html) clamped the
  interval bounds to the smallest and largest order statistics when the
  quantile index fell outside `1..n`. Barber et al. (2021) define those
  bounds as infinite, which occurs for fewer than 9 observations at
  `alpha = 0.10`. They now return `-Inf` / `Inf`, matching
  [`conformal_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_split.md).

- [`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)
  substituted the mean calibration weight for the test-point weight, so
  every test point received the same quantile. The new `weights_new`
  argument gives the exact procedure of Tibshirani et al. (2019), in
  which each test point receives its own quantile. Omitting it with
  non-uniform weights now warns.

- A fitted `glm` passed to any classification method failed with
  “missing columns for class levels”, because the internal probability
  matrix was unnamed.

- [`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md)
  failed with “‘breaks’ are not unique” whenever predictions contained
  ties. Duplicate breaks are now collapsed, with a warning reporting the
  number of bins actually used.

- Conformal functions called
  [`set.seed()`](https://rdrr.io/r/base/Random.html) on the global
  random stream and left it altered. The `seed` argument now applies for
  the duration of the call only, and the user’s `.Random.seed` is
  restored on exit. Seeded calls remain reproducible.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) failed with
  “need finite ‘ylim’ values” on unbounded intervals. These are now
  drawn to the plot edge with a message.

- [`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md)
  and
  [`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md)
  silently substituted the pooled quantile for groups with fewer than
  three calibration points, voiding the group-conditional guarantee for
  exactly the groups that needed it. Such groups now receive an
  unbounded interval, with a warning naming the number of calibration
  points required.

- Data frames containing non-numeric columns produced the misleading
  error “must not contain NaN or Inf values”. They now name the
  offending columns and suggest
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

### Changes in default behaviour

- [`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md)
  and
  [`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md)
  now default to `randomize = TRUE`, the method as published.
  Deterministic scoring (`randomize = FALSE`) remains available and now
  warns when the conformal quantile saturates at 1. Pass `seed` for
  reproducible randomised sets.

- Prediction sets are now the exact inversion of the calibrated score.
  New `allow_empty` argument on
  [`conformal_lac()`](https://charlescoverdale.github.io/predictset/reference/conformal_lac.md),
  [`conformal_aps()`](https://charlescoverdale.github.io/predictset/reference/conformal_aps.md),
  [`conformal_raps()`](https://charlescoverdale.github.io/predictset/reference/conformal_raps.md),
  [`conformal_class_split()`](https://charlescoverdale.github.io/predictset/reference/conformal_class_split.md),
  and
  [`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md);
  the default `FALSE` keeps the previous behaviour of replacing an empty
  set with the most probable class.

### Other changes

- [`conformal_cqr()`](https://charlescoverdale.github.io/predictset/reference/conformal_cqr.md)’s
  `quantiles` argument previously had no effect. It is now validated,
  recorded on the returned object, and checked for consistency with
  `alpha`.

- [`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md)
  and
  [`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md)
  now validate that `x_new` has the same number of columns as `x`.

- Jackknife+ prediction makes one call to `predict_fun` per
  leave-one-out model rather than one per (model, test point) pair.

- `conformal_cv(x_new = NULL)` records `train_approximation = TRUE`, and
  [`print()`](https://rdrr.io/r/base/print.html) states that those
  intervals do not carry the CV+ guarantee.

- `inst/CITATION` now reports the installed version rather than a
  hardcoded one.

- `paper/`, `Makefile`, and `llms*.txt` are excluded from the source
  tarball, which removes an `R CMD check` WARNING about GNU extensions
  in `paper/slides/Makefile` and cuts the tarball from 4.2 MB.

- README and the accompanying paper: corrected the CRAN status,
  dependency count, and competitor versions, and softened the Mondrian
  and ACI exclusivity claims (see `conformalForecast` and
  `AdaptiveConformal`).

## predictset 0.3.2

- Fixed [`predict()`](https://rdrr.io/r/stats/predict.html) for LAC and
  Mondrian classification objects: `randomize` field now defaults to
  `FALSE` when not set by the fitting method.

## predictset 0.3.1

- Add DOI links for all DESCRIPTION references per CRAN reviewer
  feedback.

## predictset 0.3.0

CRAN release: 2026-03-19

### Documentation

- Documented Jackknife+ and CV+ theoretical coverage guarantee
  (1-2alpha) per Barber et al. (2021)
- Documented ACI asymptotic (not finite-sample) coverage guarantee per
  Gibbs and Candes (2021)
- Documented CQR dependence on quantile model quality
- Documented deterministic vs randomized APS variants
- Added coverage guarantee footnotes to README and vignette method
  tables

### Internal

- Added `graphics` and `grDevices` to DESCRIPTION Imports
- Added missing test dependencies to Suggests

## predictset 0.2.0

### New features

- [`conformal_mondrian()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian.md)
  and
  [`conformal_mondrian_class()`](https://charlescoverdale.github.io/predictset/reference/conformal_mondrian_class.md)
  for group-conditional (Mondrian) conformal prediction
- [`conformal_weighted()`](https://charlescoverdale.github.io/predictset/reference/conformal_weighted.md)
  for weighted conformal prediction under covariate shift
- [`conformal_aci()`](https://charlescoverdale.github.io/predictset/reference/conformal_aci.md)
  for adaptive conformal inference (sequential prediction)
- [`conformal_pvalue()`](https://charlescoverdale.github.io/predictset/reference/conformal_pvalue.md)
  for conformal p-values
- [`conformal_compare()`](https://charlescoverdale.github.io/predictset/reference/conformal_compare.md)
  for benchmarking multiple methods side-by-side
- [`coverage_by_group()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_group.md)
  and
  [`coverage_by_bin()`](https://charlescoverdale.github.io/predictset/reference/coverage_by_bin.md)
  for conditional coverage diagnostics
- Progress bars via `verbose = TRUE` for
  [`conformal_jackknife()`](https://charlescoverdale.github.io/predictset/reference/conformal_jackknife.md)
  and
  [`conformal_cv()`](https://charlescoverdale.github.io/predictset/reference/conformal_cv.md)

### Improvements

- NA/NaN/Inf input validation with informative error messages
- Column dimension checks between training and test data
- Probability matrix column validation for classification methods
- Graceful handling of unseen factor levels in APS/RAPS/LAC scoring
- Negative scale model prediction warnings for normalized conformal

## predictset 0.1.0

- Initial release with split conformal, CV+, Jackknife+, CQR
  (regression) and split, APS, RAPS, LAC (classification)
