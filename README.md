
# mlmhelpr

<img src="man/figures/mlmhelpr_hex.png" align="right" height=250/>

A package of helper functions for multilevel models fit using the `lme4`
package

<!-- README.md is generated from README.Rmd. Please edit README.Rmd only -->
<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/mlmhelpr?color=red)](https://cran.r-project.org/package=mlmhelpr)
[![](https://img.shields.io/badge/status-under%20development-orange.svg)](https://github.com/lrocconi/mlmhelpr)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/lrocconi/mlmhelpr)
[![](https://img.shields.io/github/last-commit/lrocconi/mlmhelpr.svg)](https://github.com/lrocconi/mlmhelpr/commits/main)
[![CodeFactor](https://www.codefactor.io/repository/github/lrocconi/mlmhelpr/badge)](https://www.codefactor.io/repository/github/lrocconi/mlmhelpr)

<!-- badges: end -->

## Authors

-   [Louis Rocconi, PhD](https://lrocconi.github.io/)
-   [Anthony Schmidt, PhD](https://www.anthonyschmidt.co/)

## Installation

To install the latest development version directly from Github, type:

install.packages(“remotes”)

remotes::install_github(“lrocconi/mlmhelpr”)

## Functions

### `boot_se`

Compute bootstrap standard errors and confidence intervals for fixed
effects

### `center`

Automatically grand- or group-mean center variables and re-estimates the
model

### `design_effect`

Calculate the design effect to determine if multilevel modeling is
needed

### `hausman`

Perform a Hausman test to test for differences between random- and
fixed-effects models (experimental)

### `icc`

Calculate the intraclass correlation

### `ncv_tests`

Computes three different Non-constant variance tests (experimental)

### `plausible_values`

Compute the plausible value range for random effects

### `r2_cor`

Calculate the squared correlation between the observed and predicted
values

### `r2_pve`

Compute the proportion of variance explained for each random effect in
the model

### `reliability`

Calculate reliability coefficients for random effects

### `robust_se`

Computes robust standard errors for `lmer` models

### `taucov`

Calculate correlation between random intercepts and slopes
