
# edatools

<!-- badges: start -->
<!-- badges: end -->

This is a demonstration package for the book [R in Action (3rd ed.)](https://www.manning.com/books/r-in-action-third-edition).
It contains functions for exploratory data analysis.

## Installation

You can install this package with the following code:

``` r
if(!require(remotes)){
   install.packages("remotes")
}
remotes::install_github("rkabacoff/edatools")
```

## Example

This is a basic example which shows you how to describe a data frame:

``` r
library(edatools)
df_info<- contents(happiness)
df_info
plot(df_info)
```

