
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesMaxEnt

<!-- badges: start -->

<!-- badges: end -->

BayesMaxEnt is an attempt to implement a Bayesian version of MaxEnt
models of species distributions. It uses MCMC via the `nimble` package,
along with code and ideas from `maxnet`.

At the moment the vignette isn’t rendering fully, but copying and
pasting the code should work.

## Installation

You can install the development version of BayesMaxEnt from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("oharar/BayesMaxEnt")
```

## Example

This would be a basic example but it is better to check the vignette.

``` r
library(BayesMaxEnt)
#> Loading required package: maxnet
#> Loading required package: coda
#> Loading required package: nimble
#> nimble version 1.4.0 is loaded.
#> For more information on NIMBLE and a User Manual,
#> please visit https://R-nimble.org.
#> 
#> Attaching package: 'nimble'
#> The following object is masked from 'package:stats':
#> 
#>     simulate
#> The following object is masked from 'package:base':
#> 
#>     declare
## basic example code
```
