---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

The Editor
The R Journal
\bigskip

Dear Editor,
\bigskip

Please consider the article *Predictset: Conformal Prediction and Uncertainty Quantification in R* for publication in the R Journal.

The predictset package brings the full conformal-prediction framework to R behind a single model-agnostic interface. It covers eleven methods across regression, classification, and sequential prediction, including Jackknife+, CV+, conformalized quantile regression, adaptive prediction sets, Mondrian calibration for group-conditional coverage, weighted conformal for covariate shift, and adaptive conformal inference for online prediction. Several of these methods, notably Mondrian classification, weighted conformal, and ACI, have had no prior implementation on CRAN; existing R tooling via the probably package covers only a subset of regression-side procedures. The package is pure R with a minimal dependency footprint, interoperates with base model classes, ranger, xgboost, and arbitrary user-defined train/predict wrappers, and ships with diagnostics for marginal, group-conditional, and binned coverage.

The expected audience is applied statisticians, machine-learning practitioners, and domain researchers in forecasting, risk modelling, medical prediction, and algorithmic fairness who need distribution-free finite-sample coverage guarantees around their existing point-prediction models. The three-line canonical workflow and the shared interface across all eleven methods make predictset suitable both for day-to-day uncertainty quantification in production pipelines and for teaching conformal prediction in applied courses.

The manuscript has not been published in a peer-reviewed journal, is not currently under review elsewhere, and all rights to submit rest with the sole author.

\bigskip
\bigskip

Regards,
\bigskip
\bigskip

Charles Coverdale
London, United Kingdom
charles.f.coverdale@gmail.com
