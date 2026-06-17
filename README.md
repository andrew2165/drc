[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/drc)](https://cran.r-project.org/package=drc)
[![Build Status](https://travis-ci.org/DoseResponse/drc.svg?branch=master)](https://travis-ci.org/DoseResponse/drc)
[![Downloads](https://cranlogs.r-pkg.org/badges/drc)](https://cranlogs.r-pkg.org/)

# drc

## Overview

Analysis of dose-response data is made available through a suite of flexible and versatile model fitting and after-fitting functions.

## Installation


``` r
## You can install drc from GitHub
# install.packages("devtools")
## first installing drcData
devtools::install_github("DoseResponse/drcData")
## then installing the development version of drc
devtools::install_github("DoseResponse/drc")
```

## ggplot2 plotting

The `ggplotDrc()` helper creates a `ggplot2` plot from a fitted `drc`
model. It returns a regular `ggplot` object, so you can keep adding
ordinary `ggplot2` layers such as `labs()`, `lims()`, themes, and manual
colour scales.


``` r
library(drc)
library(ggplot2)

spinach.m1 <- drm(SLOPE ~ DOSE, CURVE, data = spinach, fct = LL.4())

ggplotDrc(spinach.m1) +
  labs(title = "Spinach dose-response")
```

Use `type = "confidence"` to add pointwise confidence bands around the
fitted curves.


``` r
ggplotDrc(spinach.m1, type = "confidence") +
  labs(
    title = "Spinach dose-response",
    x = "Dose",
    y = "Slope"
  )
```

The default axes are linear. Use `log_x_axis = TRUE` or
`log_y_axis = TRUE` when you want a base-10 logarithmic axis.


``` r
ggplotDrc(spinach.m1, type = "confidence", log_x_axis = TRUE)
```

Because the helper returns a `ggplot` object, formatting remains in normal
`ggplot2` style.


``` r
curve_cols <- c(
  "1" = "#D55E00",
  "2" = "#0072B2",
  "3" = "#009E73",
  "4" = "#CC79A7",
  "5" = "#E69F00"
)

ggplotDrc(spinach.m1, type = "confidence") +
  lims(x = c(0, 50)) +
  scale_colour_manual(values = curve_cols) +
  scale_fill_manual(values = curve_cols) +
  theme_minimal()
```

Curves and confidence bands are drawn from a prediction grid. The default
`gridsize` is `500`, which gives smooth curves for most use cases. Increase
it for very high-resolution output.


``` r
ggplotDrc(spinach.m1, type = "confidence", gridsize = 1000)
```
