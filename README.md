
# ntm

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The ntm enables the calculation of tranquility scores and definitions in accordance with the Natural Tranquility Method, devised by Clive Bentley (2019).  The NTM method takes four measures of the quality and quantity of sound in a space: NAMM, PONS, Lrr and Lat.  The NTM method calculates the probability of each tranquility score being measured and selects the score with the highest probability.

## Installation

You can install the latest version of ntm from [GitHub](https://github.com/drdcarpenter/ntm) with:

``` r
install_github("drdcarpenter/ntm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ntm)

## passing values for NAMM, PONS, Lrr and Lat
ntm_score(3, 50, 15, 45)

## using a dataframe with columns called NAMM, PONS, Lrr and Lat
data(ntm)
ntm(ntm)
```

