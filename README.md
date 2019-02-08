
<!-- 
README.md is generated from README.Rmd. Please edit that file
rmarkdown::render("README.Rmd") 
-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dplyrUtil)](http://cran.r-project.org/package=dplyrUtil) [![Travis-CI Build Status](https://travis-ci.org/bgctw/dplyrUtil.svg?branch=master)](https://travis-ci.org/bgctw/dplyrUtil)

Overview
--------

The `dplyrUtil` package provides functionaly to deal with tabular data composed of records of equidistant time steps.

Main functions are

-   efficient reading of parts of such files: [readEquidistantCsv](https://github.com/bgctw/dplyrUtil/tree/master/vignettes/readEquidistantCsv.md)
-   updating of grouped data.frames with ensuring equidistant time steps: [updateOutputRange](https://github.com/bgctw/dplyrUtil/tree/master/vignettes/updateOutputRange.md)

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

See the [package vignettes](https://github.com/bgctw/dplyrUtil/tree/master/vignettes) (\*.md) for examples.
