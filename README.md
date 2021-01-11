
<!-- README.md is generated from README.Rmd. Please edit that file -->

# assemblerr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/UUPharmacometrics/assemblerr.svg?branch=master)](https://travis-ci.org/UUPharmacometrics/assemblerr)
[![Codecov test
coverage](https://codecov.io/gh/UUPharmacometrics/assemblerr/branch/master/graph/badge.svg)](https://codecov.io/gh/UUPharmacometrics/assemblerr?branch=master)
<!-- badges: end -->

assemblerr is an R package to construct pharmacometric models by
combining pre-defined model components. It’s intended to simplify the
specification of complex pharmacometric models and provide a mechanism
to generate models in an automatic way. With assemblerr, models are
specified using R code and then converted to a NONMEM control stream.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("UUPharmacometrics/assemblerr", build_vignettes = TRUE)
```

## Quick start

**Load assemblerr**

``` r
library(assemblerr)
```

**Build a simple model**

``` r
m <- model() +
  prm_log_normal("v") +
  prm_log_normal("cl") +
  obs_additive(conc~amt/v*exp(-cl/v*time)) 
```

**Generate NONMEM code**

``` r
render(m) 
#> $PROBLEM
#> $INPUT ID TIME DV AMT
#> $DATA data.csv IGNORE=@
#> $PRED
#> V = THETA(1) * EXP(ETA(1))
#> CL = THETA(2) * EXP(ETA(2))
#> CONC = AMT/V * EXP(-CL/V * TIME)
#> Y = CONC + EPS(1)
#> $ESTIMATION METHOD=COND INTERACTION
#> $THETA (0, 1, Inf) ; POP_V
#> $THETA (0, 1, Inf) ; POP_CL
#> $OMEGA 0.1; IIV_V
#> $OMEGA 0.1; IIV_CL
#> $SIGMA 0.1; RUV_ADD
```

## Learning more

The best place to learn how to use assemblerr is the vignette
“Introduction”. It provides an overview of the concepts underpinning
assemblerr and helps you assembling your own models. A simple way to
find it is using the `vignette()` function in R:

``` r
vignette("introduction", package = "assemblerr")
```

## Status

assemblerr has reached a status where it can significantly simplify the
creation of pharmacometric model. Nonetheless, it is still considered in
its early stages of development. Please use the [GitHub
issue](https://github.com/UUPharmacometrics/assemblerr/issues) system if
you run into errors or have improvement suggestions.
