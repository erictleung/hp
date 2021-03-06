
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hp

<!-- badges: start -->

[![R-CMD-check](https://github.com/erictleung/hp/workflows/R-CMD-check/badge.svg)](https://github.com/erictleung/hp/actions)
<!-- badges: end -->

The goal of hp is to allow you to pipe a function into help using the
{magrittr} pipe, `%>%`.

![Demo of the h function using code shown in the examples
section](man/figures/hp.gif)

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

## Notes

The `h()` function is designed to only work interactively.

Additionally, this package was created because the {magrittr} pipe
doesn’t play well with piping in functions into the `help()` function.

This was fixed with the new native R pipe operator, `|>`.

``` r
dim |> help()
```

Note, the left side should not have parentheses or R will warn you that
the `help()` function requires, “a name, length-one character vector or
reserved word.”
