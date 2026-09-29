# Make arrow

Make arrow

## Usage

``` r
make_arrow(x0 = 1, arrow_lab, arrow_gp, col_width, xlim, x_trans = "none")
```

## Arguments

- x0:

  Position of vertical line for 0 or 1.

- arrow_lab:

  Labels for the arrows, a vector of length two.

- arrow_gp:

  Graphical parameters for arrow.

- col_width:

  Width of the column arrow to be fitted.

- xlim:

  Limits for the x-axis as a vector of length 2, i.e. `c(low, high)`. By
  default the minimum and maximum of the lower and upper values are
  used.

- x_trans:

  Scale of the axis, one of `"none"` (default), `"log"`, `"log2"` or
  `"log10"`. Use `"log"` if the values are exponential, e.g. odds ratios
  or hazard ratios. The default reference line of
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md)
  is 1 for log scales and 0 otherwise.
