
<!-- 
README.md is generated from README.Rmd. Please edit that file
rmarkdown::render(
  "README.Rmd", output_format = rmarkdown::github_document(html_preview = FALSE)) 
-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dplyrUtil)](http://cran.r-project.org/package=dplyrUtil) [![Travis-CI Build Status](https://travis-ci.org/bgctw/dplyrUtil.svg?branch=master)](https://travis-ci.org/bgctw/dplyrUtil)

Overview
--------

The `dplyrUtil` package provides utilities that deal with common task and with unequal factor levels when using dplyr.

Common tasks when using dplyr are

-   split-map-combine
-   mutate acting only on the rows satisfying the condition
-   left\_join with before dropping columns that would be duplicated

Installation
------------

``` r
# From CRAN (in future)
#install.packages("dplyrUtil")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("bgctw/dplyrUtil")
```

Usage
-----

``` r
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(dplyrUtil, quietly = TRUE)
```

Example of convenient split-map-combine with example dataset:

``` r
ds <- tibble(
  food = c("banana","strawberry","pear","bread","corn flakes")
  , kind = factor(c(rep("fruit",3), rep("cereal",2)))
  )
ds
#> # A tibble: 5 x 2
#>   food        kind  
#>   <chr>       <fct> 
#> 1 banana      fruit 
#> 2 strawberry  fruit 
#> 3 pear        fruit 
#> 4 bread       cereal
#> 5 corn flakes cereal
```

``` r
fSub <- function(dss){ mutate(dss, countInKind = paste(kind,1:n()) )}
ds %>% 
  group_by(kind) %>% 
  mapGroups(fSub)
#> # A tibble: 5 x 3
#> # Groups:   kind [2]
#>   food        kind   countInKind
#>   <chr>       <fct>  <chr>      
#> 1 bread       cereal cereal 1   
#> 2 corn flakes cereal cereal 2   
#> 3 banana      fruit  fruit 1    
#> 4 strawberry  fruit  fruit 2    
#> 5 pear        fruit  fruit 3
```

Compare this to the usual nest-map-unnest idiom:

``` r
fSub2 <- function(dss, kind){ mutate(dss, countInKind = paste(kind,1:n()) )}
ds %>% 
  group_by(kind) %>% 
  nest() %>% 
  mutate(data = purrr::map2(data, kind, fSub2)) %>% 
  unnest()
#> # A tibble: 5 x 3
#>   kind   food        countInKind
#>   <fct>  <chr>       <chr>      
#> 1 fruit  banana      fruit 1    
#> 2 fruit  strawberry  fruit 2    
#> 3 fruit  pear        fruit 3    
#> 4 cereal bread       cereal 1   
#> 5 cereal corn flakes cereal 2
```

See the [usecase vignette](https://github.com/bgctw/dplyrUtil/blob/master/vignettes/dplyrUtilUsage.md) and other [package vignettes](https://github.com/bgctw/dplyrUtil/tree/master/vignettes) (\*.md) for examples.
