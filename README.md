
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hp

<!-- badges: start -->

[![R-CMD-check](https://github.com/erictleung/hp/workflows/R-CMD-check/badge.svg)](https://github.com/erictleung/hp/actions)
<!-- badges: end -->

The goal of hp is to allow you to pipe a function into help.

## Installation

You can install the released version of hp from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hp")
```

And the development version from [GitHub](https://github.com/) with:

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