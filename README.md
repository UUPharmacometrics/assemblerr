
<!-- README.md is generated from README.Rmd. Please edit that file -->

# assemblerr

assemblerr is an R package to construct pharmacometric models by
combining pre-defined model components. It’s intended to simplify the
specification of complex pharmacometric models and provide a mechanism
to generate models in an automatic way. With assemblerr, models are
specified using R code and then converted to a NONMEM control stream.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UUPharmacometrics/assemblerr")
```

## Example

This is a basic example which shows you how to create an Emax model and
generate the corresponding NONMEM code:

``` r
library(assemblerr)

m <- model() +
  prm_log_normal("emax") +
  prm_normal("ed50") +
  obs_additive(effect~emax*dose/(ed50+dose)) 

render(m) 
#> $INPUT ID TIME DV AMT
#> $DATA data.csv IGNORE=@
#> 
#> $PRED
#> MU_1 = LOG(THETA(1))
#> EMAX = THETA(1) * EXP(ETA(1))
#> MU_2 = THETA(2)
#> ED50 = THETA(2) + ETA(2)
#> EFFECT = EMAX * DOSE/(ED50 + DOSE)
#> Y = EFFECT + EPS(1)
#> $THETA (0, 1, Inf) ; POP_EMAX
#> $THETA (0, 1, Inf) ; POP_ED50
#> $OMEGA 0.1; IIV_EMAX
#> $OMEGA 0.1; IIV_ED50
#> $SIGMA 0.1
```
