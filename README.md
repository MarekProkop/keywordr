
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The keywordr SEO package for R

<!-- badges: start -->

<!-- badges: end -->

The *keywordr* package provides an efficient and user-friendly framework
for keyword research. The results are then typically used to optimize
websites for search engines, create content strategy, design information
architecture for websites, etc.

## Installation

You can install the development version of keywordr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MarekProkop/keywordr", build_vignettes = TRUE)
```

Note: You must have [R
version 4.1.0](https://www.r-bloggers.com/2021/05/new-features-in-r-4-1-0/)
because the package makes use of the native R pipe and the new syntax
for anonymous functions.

## Example

This is a basic example which shows you how to use the package:

### Create a `kwresearch` object and import queries

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
#> ✔ tibble  3.1.7     ✔ dplyr   1.0.9
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.0
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(keywordr)

input_data <- tribble(
  ~query,             ~volume,
  "seo",                96000,
  "seo ye-ji",          22000,
  "seo meaning",         6700,
  "seo services",        6400,
  "what is a seo",       5300,
  "seo london",          5000,
  "what is seo",         4800,
  "seo agency",          4300,
)

kwr <- kwresearch(input_data)

kwr_queries(kwr)
#> # A tibble: 8 × 7
#>   query_normalized n_queries volume   cpc query_original input source
#>   <chr>                <int>  <dbl> <dbl> <chr>          <chr> <chr> 
#> 1 seo                      1  96000    NA seo            <NA>  <NA>  
#> 2 seo ye-ji                1  22000    NA seo ye-ji      <NA>  <NA>  
#> 3 seo meaning              1   6700    NA seo meaning    <NA>  <NA>  
#> 4 seo services             1   6400    NA seo services   <NA>  <NA>  
#> 5 what is a seo            1   5300    NA what is a seo  <NA>  <NA>  
#> 6 seo london               1   5000    NA seo london     <NA>  <NA>  
#> 7 what is seo              1   4800    NA what is seo    <NA>  <NA>  
#> 8 seo agency               1   4300    NA seo agency     <NA>  <NA>
```

### Prune unwanted queries

``` r
recipe_file <- file.path(tempdir(), "recipes.yml")

kwr_add_pattern("ye-ji", recipe_file, recipe_type = "remove")

kwr <- kwr |> 
  kwr_prune(recipe_file)
#> Removing queries...
#> Removed 1 queries out of 8. Duration: 0.008s

kwr_queries(kwr)
#> # A tibble: 7 × 7
#>   query_normalized n_queries volume   cpc query_original input source
#>   <chr>                <int>  <dbl> <dbl> <chr>          <chr> <chr> 
#> 1 seo                      1  96000    NA seo            <NA>  <NA>  
#> 2 seo meaning              1   6700    NA seo meaning    <NA>  <NA>  
#> 3 seo services             1   6400    NA seo services   <NA>  <NA>  
#> 4 what is a seo            1   5300    NA what is a seo  <NA>  <NA>  
#> 5 seo london               1   5000    NA seo london     <NA>  <NA>  
#> 6 what is seo              1   4800    NA what is seo    <NA>  <NA>  
#> 7 seo agency               1   4300    NA seo agency     <NA>  <NA>
```

### Explore queries

``` r
kwr |> kwr_ngrams()
#> # A tibble: 8 × 3
#>   token             n volume
#>   <chr>         <int>  <dbl>
#> 1 seo               7 128500
#> 2 what is           2  10100
#> 3 seo meaning       1   6700
#> 4 seo services      1   6400
#> 5 what is a seo     1   5300
#> 6 seo london        1   5000
#> 7 what is seo       1   4800
#> 8 seo agency        1   4300
```

### Classify queries

``` r
kwr_add_pattern(
  pattern = "agenc",
  recipe_file = recipe_file,
  recipe_type = "label",
  dim_name = "bussiness_type",
  value = "agency"
)
kwr_add_pattern(
  pattern = "meaning",
  recipe_file = recipe_file,
  recipe_type = "label",
  dim_name = "info"
)
kwr_add_pattern(
  pattern = "what is",
  recipe_file = recipe_file,
  recipe_type = "label",
  dim_name = "info"
)

kwr <- kwr |> 
  kwr_classify(recipe_file)
#> Label:bussiness_type
#>   Value: agency
#> Label:info

kwr_queries(kwr) |> 
  select(1:5)
#> # A tibble: 7 × 5
#>   query_normalized bussiness_type info    n_queries volume
#>   <chr>            <chr>          <chr>       <int>  <dbl>
#> 1 seo              <NA>           <NA>            1  96000
#> 2 seo meaning      <NA>           meaning         1   6700
#> 3 seo services     <NA>           <NA>            1   6400
#> 4 what is a seo    <NA>           what is         1   5300
#> 5 seo london       <NA>           <NA>            1   5000
#> 6 what is seo      <NA>           what is         1   4800
#> 7 seo agency       agency         <NA>            1   4300
```

Please see a more detailed example in the vignette Recomended workflow.
