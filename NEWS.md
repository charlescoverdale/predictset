# predictset 0.3.2

* Fixed `predict()` for LAC and Mondrian classification objects: `randomize`
  field now defaults to `FALSE` when not set by the fitting method.

# predictset 0.3.1

* Add DOI links for all DESCRIPTION references per CRAN reviewer feedback.

# predictset 0.3.0

## Documentation
- Documented Jackknife+ and CV+ theoretical coverage guarantee (1-2alpha) per Barber et al. (2021)
- Documented ACI asymptotic (not finite-sample) coverage guarantee per Gibbs and Candes (2021)
- Documented CQR dependence on quantile model quality
- Documented deterministic vs randomized APS variants
- Added coverage guarantee footnotes to README and vignette method tables

## Internal
- Added `graphics` and `grDevices` to DESCRIPTION Imports
- Added missing test dependencies to Suggests

# predictset 0.2.0

## New features
- `conformal_mondrian()` and `conformal_mondrian_class()` for group-conditional (Mondrian) conformal prediction
- `conformal_weighted()` for weighted conformal prediction under covariate shift
- `conformal_aci()` for adaptive conformal inference (sequential prediction)
- `conformal_pvalue()` for conformal p-values
- `conformal_compare()` for benchmarking multiple methods side-by-side
- `coverage_by_group()` and `coverage_by_bin()` for conditional coverage diagnostics
- Progress bars via `verbose = TRUE` for `conformal_jackknife()` and `conformal_cv()`

## Improvements
- NA/NaN/Inf input validation with informative error messages
- Column dimension checks between training and test data
- Probability matrix column validation for classification methods
- Graceful handling of unseen factor levels in APS/RAPS/LAC scoring
- Negative scale model prediction warnings for normalized conformal

# predictset 0.1.0
- Initial release with split conformal, CV+, Jackknife+, CQR (regression) and split, APS, RAPS, LAC (classification)
