
<!-- README.md is generated from README.Rmd. Please edit that file -->

# assemblerr

<!-- badges: start -->

[![R-CMD-check](https://github.com/UUPharmacometrics/assemblerr/workflows/R-CMD-check/badge.svg)](https://github.com/UUPharmacometrics/assemblerr/actions)
[![Codecov test
coverage](https://codecov.io/gh/UUPharmacometrics/assemblerr/branch/master/graph/badge.svg)](https://codecov.io/gh/UUPharmacometrics/assemblerr?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/assemblerr)](https://CRAN.R-project.org/package=assemblerr)
<!-- badges: end -->

assemblerr is an R package to construct pharmacometric models by
combining pre-defined model components. It’s intended to simplify the
specification of complex pharmacometric models and provide a mechanism
to generate models in an automatic way. With assemblerr, models are
specified using R code and then converted to a NONMEM control stream.

## Installation

You can install the latest CRAN version `assemblerr` using:

``` r
install.packages("assemblerr")
```

## Quick start

**Load assemblerr**

``` r
library(assemblerr)
```

**Build a simple model**

``` r
m <- model() +
  input_variable("dose") +
  prm_log_normal("emax", median = 10, var_log = 0.09) +
  prm_log_normal("ed50", median = 50, var_log = 0.09) +
  algebraic(effect~emax*dose/(ed50 + dose)) +
  obs_additive(~effect, var_add = 1)
```

**Generate NONMEM code**

``` r
render(m) 
#> $PROBLEM
#> $INPUT DOSE ID DV
#> $DATA data.csv IGNORE=@
#> $PRED
#> EMAX = THETA(1) * EXP(ETA(1))
#> ED50 = THETA(2) * EXP(ETA(2))
#> EFFECT = EMAX * DOSE/(ED50 + DOSE)
#> Y = EFFECT + EPS(1)
#> $ESTIMATION METHOD=COND MAXEVAL=999999 
#> $THETA (0, 10, Inf) ; POP_EMAX
#> $THETA (0, 50, Inf) ; POP_ED50
#> $OMEGA 0.09 ; IIV_EMAX
#> $OMEGA 0.09 ; IIV_ED50
#> $SIGMA 1; RUV_ADD
```

## Learning more

The best place to learn how to use assemblerr is the vignette “Getting
Started”. It provides an overview of the functionality in assemblerr and
helps you building your own models. A simple way to find it is using the
`vignette()` function in R:

``` r
vignette("getting-started", package = "assemblerr")
```
