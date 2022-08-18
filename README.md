
# mlmhelpr

<img src="man/figures/mlmhelpr_hex.png" align="right" height=250/>

A package of helper functions for multilevel models fit using the `lme4`
package

<!-- README.md is generated from README.Rmd. Please edit README.Rmd only -->
<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/mlmhelpr?color=red)](https://cran.r-project.org/package=mlmhelpr)
[![](https://img.shields.io/badge/status-under%20development-orange.svg)](https://github.com/lrocconi/mlmhelpr)
[![](https://img.shields.io/badge/devel%20version-0.1-blue.svg)](https://github.com/lrocconi/mlmhelpr)
[![](https://img.shields.io/github/last-commit/lrocconi/mlmhelpr.svg)](https://github.com/lrocconi/mlmhelpr/commits/main)
[![CodeFactor](https://www.codefactor.io/repository/github/lrocconi/mlmhelpr/badge)](https://www.codefactor.io/repository/github/lrocconi/mlmhelpr)

<!-- badges: end -->

## Authors

-   [Louis Rocconi, PhD](https://lrocconi.github.io/)
-   [Anthony Schmidt, PhD](http://www.anthonyschmidt.co)

## Installation

This package is currently under development. Check back later.

## Functions

### `boot_se`

Compute bootstrap standard errors and confidence intervals for fixed
effects

### `center`

Automatically grand- or group-mean center variables and re-estimates the
model

### `hausman`

Perform a Hausman test to test for differences between random- and
fixed-effects models (experimental)

### `design_effect`

Calculate the design effect to determine if multilevel modeling is
needed

### `icc`

Calculate the intraclass correlation

### `plausible_values`

Compute the plausible value range for random effects

### `pve`

Compute the proportion of variance explained for each random effect in
the model

### `r2`

Calculate several
pseudo-![r^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r%5E2 "r^2")
values

### `r2_cor`

Calculate the squared correlation between the observed and predicted
values

### `robust_se`

Computes robust standard errors for `lmer` models

### `taucov`

Calculate correlation between random intercepts and slopes
