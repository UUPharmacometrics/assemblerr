
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
remotes::install_github("UUPharmacometrics/assemblerr", build_opts = c("--no-resave-data", "--no-manual"))
```

## Example

This is a basic example which shows you how to create a simple analytic
model and generate the corresponding NONMEM code:

``` r
library(assemblerr)

m <- model() +
  prm_log_normal("v") +
  prm_log_normal("cl") +
  obs_additive(conc~amt/v*exp(-cl/v*time)) 

render(m) 
#> $INPUT ID TIME DV AMT
#> $DATA data.csv IGNORE=@
#> 
#> $PRED
#> MU_1 = LOG(THETA(1))
#> V = THETA(1) * EXP(ETA(1))
#> MU_2 = LOG(THETA(2))
#> CL = THETA(2) * EXP(ETA(2))
#> CONC = AMT/V * EXP(-CL/V * TIME)
#> Y = CONC + EPS(1)
#> $THETA (0, 1, Inf) ; POP_V
#> $THETA (0, 1, Inf) ; POP_CL
#> $OMEGA 0.1; IIV_V
#> $OMEGA 0.1; IIV_CL
#> $SIGMA 0.1
```
