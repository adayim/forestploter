
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forestploter

<!-- badges: start -->
<!-- badges: end -->

The goal of forestploter is to create a publication-ready forest plot
with little effort. This package provide some extra displays compared to
other packages.

## Installation

You can install the development version of forestploter from
[GitHub](https://github.com/adayim/forestploter) with:

``` r
# install.packages("devtools")
devtools::install_github("adayim/forestploter")
```

## Basic usage

This is a basic example which shows you how to create a forestplot:

``` r
library(forestploter)

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Add blank column for the forest plot to display CI
dt$` ` <- paste(rep(" ", 5), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est, dt$low, dt$hi))

p <- forest(dt[,c(1:3, 20:21)],
            est = dt$est,
            lower = dt$low, 
            upper = dt$hi,
            sizes = dt$se,
            ci.column = 4,
            ref.line = 1,
            ci.column.width = 2,
            arrow.lab = c("Placebo Better", "Treatment Better"),
            tick.breaks = c(0.5, 1, 2, 4))

# Draw plot
grid::grid.newpage()
grid::grid.draw(p)
```

<img src="man/figures/README-example-1.png" width="100%" height="80%" />

## Complex usage

This is an example of multiple CI columns and groups:

``` r
# Add blank column for the second CI column
dt$`   ` <- paste(rep(" ", 5), collapse = " ")

p <- forest(dt[,c(1:2, 20, 3, 22)],
            est = list(dt$est_gp1,
                       dt$est_gp2,
                       dt$est_gp3,
                       dt$est_gp4),
            lower = list(dt$low_gp1,
                         dt$low_gp2,
                         dt$low_gp3,
                         dt$low_gp4), 
            upper = list(dt$hi_gp1,
                         dt$hi_gp2,
                         dt$hi_gp3,
                         dt$hi_gp4),
            ci.column = c(3, 5),
            ref.line = 1,
            ci.column.width = 2,
            ci.color = c("Group 1" = "#e41a1c", "Group 2" = "#4daf4a"),
            legend = list(name = "Group",
                          position = "bottom"),
            arrow.lab = c("Placebo Better", "Treatment Better"),
            nudge_y = 0.2)

# Draw plot
grid::grid.newpage()
grid::grid.draw(p)
```

<img src="man/figures/README-multiple-1.png" width="100%" />
