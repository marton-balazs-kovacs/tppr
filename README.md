
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tppr <a href='https://marton-balazs-kovacs.github.io/tppr/'><img src='man/figures/logo.png' align="right" height="200" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of tppr is to gather all the functions that are needed to
recreate the results of the [Transparent Psi
Project](https://osf.io/jk2zf/). The analysis can be run locally from R,
but the package also includes a Shiny app, that allows people interested
in the project to follow the results of the study real-time.

## Installation

You can install the development version (tppr is not available on CRAN)
from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("marton-balazs-kovacs/tppr")
```

## Using the app

The Shiny app is hosted on: ADD LINK

However, you can run the app locally.

``` r
tppr::run_app()
```

## Using the package

You can read more about how the functions included in the package work
in `vignette("use_pkg")`.

Features coming soon:

  - Reproducing the paper locally

## Code of Conduct

Please note that the tppr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
