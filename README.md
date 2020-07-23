
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spccharter

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/johnmackintosh/spccharter.svg?branch=master)](https://travis-ci.com/johnmackintosh/spccharter)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/johnmackintosh/spccharter?branch=master&svg=true)](https://ci.appveyor.com/project/johnmackintosh/spccharter)
[![R build
status](https://github.com/johnmackintosh/spccharter/workflows/R-CMD-check/badge.svg)](https://github.com/johnmackintosh/spccharter/actions)
<!-- badges: end -->

The goal of spccharter is rapid analysis of multiple statistical process
control charts. The package will create charts, detect signals of
improvement, and revise control limits each time a signal occurs.
Currently, ‘C’, ‘P’ and ‘U’ charts can be produced.

The package supports non standard evaluation - you can pass bare
variable names.

## Installation

spccharter is not on [CRAN](https://CRAN.R-project.org) yet

*But* the development version is available on
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes") # if not already installed
remotes::install_github("johnmackintosh/spccharter")
```

## Example

One grouping variable:

``` r
library(spccharter)
spccharter(data, numerator = counts, datecol = date, 
           by = category_1, plot_type = 'c', direction = "both")
```

Two grouping variables:

You might need to experiment with the order of the variables. Usually,
passing the lowest level (e.g. Ward, then Hospital) works best.

``` r
library(spccharter)
spccharter(data, numerator = counts, denominator = attends, datecol = date, 
           by = c('ward','hospital'), plot_type = 'p', direction = "both")
```

## Example plots

![facet-spccharter](https://user-images.githubusercontent.com/3278367/84841170-7a516300-b039-11ea-90fb-9a373ac8bc26.PNG)

![spc-c-chart](https://user-images.githubusercontent.com/3278367/84840888-b932e900-b038-11ea-87d0-2e32e99bcdd1.png)

![spc-pchart](https://user-images.githubusercontent.com/3278367/84840901-c3ed7e00-b038-11ea-9377-b7a564433af0.png)
