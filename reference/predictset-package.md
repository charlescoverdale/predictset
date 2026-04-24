# predictset: Conformal Prediction and Uncertainty Quantification

Implements conformal prediction methods for constructing prediction
intervals (regression) and prediction sets (classification) with
finite-sample coverage guarantees. Methods include split conformal,
'CV+' and 'Jackknife+' (Barber et al. 2021)
[doi:10.1214/20-AOS1965](https://doi.org/10.1214/20-AOS1965) ,
'Conformalized Quantile Regression' (Romano et al. 2019)
[doi:10.48550/arXiv.1905.03222](https://doi.org/10.48550/arXiv.1905.03222)
, 'Adaptive Prediction Sets' (Romano, Sesia, Candes 2020)
[doi:10.48550/arXiv.2006.02544](https://doi.org/10.48550/arXiv.2006.02544)
, 'Regularized Adaptive Prediction Sets' (Angelopoulos et al. 2021)
[doi:10.48550/arXiv.2009.14193](https://doi.org/10.48550/arXiv.2009.14193)
, Mondrian conformal prediction for group-conditional coverage (Vovk,
Gammerman, and Shafer 2005)
[doi:10.1007/b106715](https://doi.org/10.1007/b106715) , weighted
conformal prediction for covariate shift (Tibshirani et al. 2019)
[doi:10.48550/arXiv.1904.06019](https://doi.org/10.48550/arXiv.1904.06019)
, and adaptive conformal inference for sequential prediction (Gibbs and
Candes 2021)
[doi:10.48550/arXiv.2106.00170](https://doi.org/10.48550/arXiv.2106.00170)
. All methods are distribution-free and provide calibrated uncertainty
quantification without parametric assumptions. Works with any model that
can produce predictions from new data, including 'lm', 'glm', 'ranger',
'xgboost', and custom user-defined models.

## See also

Useful links:

- <https://github.com/charlescoverdale/predictset>

- Report bugs at <https://github.com/charlescoverdale/predictset/issues>

## Author

**Maintainer**: Charles Coverdale <charlesfcoverdale@gmail.com>
\[copyright holder\]
