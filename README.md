
<!-- README.md is generated from README.Rmd. Please edit that file -->

# changepoint.forecast

<!-- badges: start -->
<!-- badges: end -->

The goal of changepoint.forecast is to perform sequential changepoint
analysis of forecast errors in order to quickly detect when forecasts
become inaccurate.

## Installation

<!-- You can install the released version of changepoint.forecast from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("changepoint.forecast")
```
-->

The development version can be downloaded from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("grundy95/changepoint.forecast")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(changepoint.forecast)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
