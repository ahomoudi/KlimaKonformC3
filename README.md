# KlimaKonformC3

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL
v3.0](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/ahmathlete/KlimaKonformC3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahmathlete/KlimaKonformC3/actions/workflows/R-CMD-check.yaml)
![GitHub R package
version](https://img.shields.io/github/r-package/v/ahmathlete/KlimaKonformC3)
![GitHub last commit](https://img.shields.io/github/last-commit/ahmathlete/KlimaKonformC3)
![GitHub commits since tagged version](https://img.shields.io/github/commits-since/ahmathlete/KlimaKonformC3/v2.0.0?style=plastic)

<!-- badges: end -->
<img align="right" width="200" src="man/figures/logo.png">

The goal of KlimaKonformC3 is to analyse the Agricultural Simulations
performed within of the Project KlimaKonform (TB C3 Landwirtschaft). The
project homepage is [here](https://klimakonform-dmp.geo.tu-dresden.de/).

## Installation

`KlimaKonformC3` is NOT available from CRAN, so you can NOT use
`install.packages("KlimaKonformC3")`.

You can install the development version of KlimaKonformC3 like so:

``` r
# Intsalling KlimaKonfromC3
devtools::install_github("ahmathlete/KlimaKonformC3")
```

## Infromation

### Functions

Many types of functions are included in the package:

1\. `sim` functions: they are mainly written to perform analysis on
    individual simulation files/file.

2\. `ens` functions: they are mainly written to perform analysis on
ensemble files/file.

3\. `clima` functions: they are mainly written to perform analysis of
climate projections used in agricultural simulations.


### Production

All R scripts used in production are included in `production` folder. 
``` r
library(KlimaKonformC3)
## basic example code
```

## Contact

Please file bug reports and feature requests at
<https://github.com/ahmathlete/KlimaKonformC3/issues>


## Disclaimer
The Original project logo is on the official project website mentioned above. The logo here is modified with @ColinFay's awesome [hexmake](https://connect.thinkr.fr/hexmake/) tool.
