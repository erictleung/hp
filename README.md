
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hp

<!-- badges: start -->

[![R-CMD-check](https://github.com/erictleung/hp/workflows/R-CMD-check/badge.svg)](https://github.com/erictleung/hp/actions)
<!-- badges: end -->

![Demo of the h function using code shown in the examples
section](man/figures/hp.gif)

The goal of hp is to allow you to pipe a function into help.

## Installation

The development version from [GitHub](https://github.com/erictleung/hp)
with:

``` r
# install.packages("devtools")
remotes::install_github("erictleung/hp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hp)
library(dplyr)

# Simple single function
t.test %>% h

# Simple single function with parentheses
dim() %>% h

# Function but specifying package
dplyr::across %>% h

# Function but specifying package with parentheses
dplyr::summarise() %>% h
```

The `h()` function is designed to only work interactively.
