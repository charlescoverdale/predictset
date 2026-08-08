## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- macOS Tahoe 26.5 (local, aarch64), R 4.5.2, `--as-cran --run-donttest`

Not yet checked on win-builder or R-devel; run those before submitting.

## Submission notes

This release fixes three defects that changed numerical results:

- `conformal_aci()` applied the Gibbs and Candes (2021) online update with the
  operands reversed, so miscoverage narrowed rather than widened the next
  interval.
- The `model` argument was ignored when supplied as a formula or a fitted
  model object.
- `conformal_aps()` / `conformal_raps()` could return the full label set for
  every observation.

Defaults changed for `randomize` in `conformal_aps()` and `conformal_raps()`,
and new `weights_new` and `allow_empty` arguments were added. All changes are
documented in NEWS.md. There are no reverse dependencies.
