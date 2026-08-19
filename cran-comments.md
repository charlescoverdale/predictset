# CRAN submission comments: predictset 0.4.0

## Reason for this submission

This is a bug-fix release for predictset 0.3.0, currently on CRAN. It
corrects several defects that changed numerical results, and changes two
defaults to match the methods as published.

Anyone using `conformal_aci()`, `conformal_aps()`, `conformal_raps()`,
`conformal_weighted()` or `conformal_mondrian()`, or passing `model` as
a formula or a fitted model object, should re-run their analysis.

## Defects that changed results

* `conformal_aci()` applied the Gibbs and Candes (2021, Eq. 2) online
  update with the operands reversed. A miscoverage event therefore
  narrowed the next interval instead of widening it, turning the
  intended negative feedback into positive feedback, and `alpha_t` ran
  away to a clip boundary. Under a variance shift this drove empirical
  coverage to 0.605 against a 0.90 target.

* The `model` argument was discarded whenever it was a formula or a
  fitted model object, and a plain `lm(y ~ .)` was fitted in its place.
  `model = y ~ a` silently fitted every column of `x`, and
  `lm(y ~ poly(v1, 3) + v2)` was refitted as `y ~ v1 + v2`. Formulas and
  fitted models are now honoured and refitted on each conformal split.
  Objects that cannot be refitted now raise an error naming
  `make_model()` rather than silently substituting a default.

* `conformal_aps()` and `conformal_raps()` could return the full label
  set for every observation. The set builder included the class that
  crossed the threshold rather than inverting the calibrated score, and
  `randomize = TRUE` randomised the calibration scores but never the set
  construction. With oracle probabilities on a four-class problem, APS
  returned a mean set size of 3.90 out of 4 at 99.9% coverage. It now
  returns 2.69 at 88.6%.

* `conformal_weighted()` substituted the mean calibration weight for the
  test-point weight, so every test point received the same quantile. The
  new `weights_new` argument gives the exact procedure of Tibshirani et
  al. (2019), in which each test point receives its own quantile.
  Omitting it with non-uniform weights now warns.

* `conformal_mondrian()` and `conformal_mondrian_class()` silently
  substituted the pooled quantile for groups with fewer than three
  calibration points, voiding the group-conditional guarantee for
  exactly the groups that needed it. Such groups now receive an
  unbounded interval, with a warning naming the number of calibration
  points required.

* `conformal_jackknife()`, `conformal_cv()` and `predict()` clamped
  interval bounds to the smallest and largest order statistics when the
  quantile index fell outside `1..n`. Barber et al. (2021) define those
  bounds as infinite, which occurs for fewer than 9 observations at
  `alpha = 0.10`. They now return `-Inf` / `Inf`, matching
  `conformal_split()`.

Smaller fixes (a fitted `glm` failing against the classification
methods, `coverage_by_bin()` failing on tied predictions, `plot()`
failing on unbounded intervals, and clearer errors for non-numeric
columns) are listed in NEWS.md.

## Changes in default behaviour

* `conformal_aps()` and `conformal_raps()` now default to
  `randomize = TRUE`, the method as published. Deterministic scoring
  remains available via `randomize = FALSE`, and now warns when the
  conformal quantile saturates at 1.

* Prediction sets are now the exact inversion of the calibrated score.
  The new `allow_empty` argument defaults to `FALSE`, which preserves
  the previous behaviour of replacing an empty set with the most
  probable class.

* Conformal functions previously called `set.seed()` on the global
  random stream and left it altered. The `seed` argument now applies for
  the duration of the call only, and the user's `.Random.seed` is
  restored on exit. Seeded calls remain reproducible.

## R CMD check results

0 errors | 0 warnings | 0 notes

Local check: macOS (aarch64), R 4.5.2, `devtools::check(cran = TRUE)`,
run 19 August 2026.

The local run also emits "checking for future file timestamps: unable to
verify current time". That is the usual artefact of the checking machine
being unable to reach worldclockapi.com, and is unrelated to the
package.

`paper/`, `Makefile` and `llms*.txt` are now excluded from the source
tarball. This clears an `R CMD check` WARNING about GNU extensions in
`paper/slides/Makefile`, and takes the tarball from 4.2 MB to 101 KB.

## Downstream dependencies

None.
