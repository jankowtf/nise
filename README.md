
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nise

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/valid)](https://CRAN.R-project.org/package=valid)
<!-- badges: end -->

## Installation

You can install the development version of from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rappster/nise")
```

## What?

Helpers for NSE:

-   `handle_nse_name()`: meant for values that denote column or list
    element names. It returns an object of class `name` “no matter what
    you throw at it”. Currently you can throwvalues of class
    `name`/`symbol`, `character`, `quosure`.
-   `handle_nse_names()`: “vectorized” version of `handle_nse_name()`

## Why?

I was looking for something that works well

-   with the NSE mechanism implemented by `dplyr` and its tidyverse
    friends as well as the base R mechanism for subsetting column/list
    element names

-   inside nested function calls

## How?

``` r
library(nise)
```

Example data

``` r
data_tbl <- tibble::tribble(
    ~color,     ~a, ~b, ~c, ~d,
    "blue",      1,  2, TRUE, FALSE,
    "green",     6,  2, TRUE, FALSE,
    "purple",    3,  3, TRUE, FALSE,
    "red",       2,  3, TRUE, FALSE,
    "yellow",    5,  1, TRUE, FALSE
)

data_lst <- data_tbl %>% as.list()
```

### Functions `handle_nse_name()` and `handle_nse_names()`

They are typically called **within** other functions to handle argument
values of different types for column names in a data frame/tibble or
names of a list.

The goal is to no matter how the value is supplied a `name` should be
returned as this seems to be the best “processed value” that plays
nicely with the NSE mechanism implemented in `dplyr` and friends.

#### Explicit arguments

``` r
foo <- function(data, col) {
    col <- nise::handle_nse_name(col)
    data %>% dplyr::select(!!col)
}

data_tbl %>% foo(color)
#> # A tibble: 5 × 1
#>   color 
#>   <chr> 
#> 1 blue  
#> 2 green 
#> 3 purple
#> 4 red   
#> 5 yellow
data_tbl %>% foo("color")
#> # A tibble: 5 × 1
#>   color 
#>   <chr> 
#> 1 blue  
#> 2 green 
#> 3 purple
#> 4 red   
#> 5 yellow
data_tbl %>% foo(rlang::quo(color))
#> # A tibble: 5 × 1
#>   color 
#>   <chr> 
#> 1 blue  
#> 2 green 
#> 3 purple
#> 4 red   
#> 5 yellow

tmp <- rlang::sym("color")
col <- rlang::enquo(tmp)
data_tbl %>% foo(col)
#> # A tibble: 5 × 1
#>   color 
#>   <chr> 
#> 1 blue  
#> 2 green 
#> 3 purple
#> 4 red   
#> 5 yellow
```

Also works for lists

``` r
foo_list <- function(data, name) {
    name <- nise::handle_nse_name(name)
    data %>% `[[`(name)
}
data_lst %>% foo_list(color)
#> [1] "blue"   "green"  "purple" "red"    "yellow"
data_lst %>% foo_list("color")
#> [1] "blue"   "green"  "purple" "red"    "yellow"
data_lst %>% foo_list(rlang::quo(color))
#> [1] "blue"   "green"  "purple" "red"    "yellow"

tmp <- rlang::sym("color")
col <- rlang::enquo(tmp)
data_lst %>% foo_list(col)
#> [1] "blue"   "green"  "purple" "red"    "yellow"
```

#### Implicit arguments using `...`

``` r
bar <- function(data, ...) {
    cols <- nise::handle_nse_names(...)
    data %>% dplyr::select(!!!cols)
}
col_c <- rlang::sym("c")
col_d <- "d"
col_d_enquo <- rlang::enquo(col_d)

data_tbl %>% bar(b, rlang::quo(a), "color", col_d_enquo, col_c)
#> # A tibble: 5 × 5
#>       b     a color  d     c    
#>   <dbl> <dbl> <chr>  <lgl> <lgl>
#> 1     2     1 blue   FALSE TRUE 
#> 2     2     6 green  FALSE TRUE 
#> 3     3     3 purple FALSE TRUE 
#> 4     3     2 red    FALSE TRUE 
#> 5     1     5 yellow FALSE TRUE
```

Also works for lists

``` r
bar_list <- function(data, ...) {
    names <- nise::handle_nse_names(...) %>% as.character()
    data %>% `[`(names)
}
col_c <- rlang::sym("c")
col_d <- "d"
col_d_enquo <- rlang::enquo(col_d)

data_lst %>% bar_list(b, rlang::quo(a), "color", col_d_enquo, col_c)
#> $b
#> [1] 2 2 3 3 1
#> 
#> $a
#> [1] 1 6 3 2 5
#> 
#> $color
#> [1] "blue"   "green"  "purple" "red"    "yellow"
#> 
#> $d
#> [1] FALSE FALSE FALSE FALSE FALSE
#> 
#> $c
#> [1] TRUE TRUE TRUE TRUE TRUE
```

#### Just looking at `handle_nse_name()` and `handle_nse_names()`

``` r
rlang::sym("color") %>% handle_nse_name()
#> color
rlang::quo(color) %>% handle_nse_name()
#> color
"color" %>% handle_nse_name()
#> color
character() %>% handle_nse_name()
#> character(0)

tmp <- rlang::sym("color")
col <- rlang::enquo(tmp)
handle_nse_name(col)
#> color
```

``` r
var <- rlang::sym("color")
var %>% handle_nse_name()
#> color

var <- rlang::quo(color)
var %>% handle_nse_name()
#> color

var <- "color" 
var %>% handle_nse_name()
#> color

var <- character()
var %>% handle_nse_name()
#> character(0)

tmp <- rlang::sym("color")
var <- rlang::enquo(tmp)
var %>% handle_nse_name()
#> color
```
