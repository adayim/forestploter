# Create x-axis

This function used to x-axis for the forest plot.

## Usage

``` r
make_xaxis(
  at,
  at_minor = NULL,
  xlab = NULL,
  x0 = 1,
  x_trans = "none",
  ticks_digits = 1,
  gp = gpar(),
  xlab_gp = NULL,
  xlim
)
```

## Arguments

- at:

  Numerical vector, create ticks at given values.

- at_minor:

  Numerical vector, create ticks at given values without label.

- xlab:

  X-axis label, see
  [`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md).

- x0:

  Position of vertical line for 0 or 1.

- x_trans:

  Scale of the axis, one of `"none"` (default), `"log"`, `"log2"` or
  `"log10"`. Use `"log"` if the values are exponential, e.g. odds ratios
  or hazard ratios. The default reference line of
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md)
  is 1 for log scales and 0 otherwise.

- ticks_digits:

  Number of digits for the tick labels. If an integer is given, for
  example `1L`, trailing zeros after the decimal mark are dropped. Give
  a double, for example `1`, to keep them. By default the number is
  calculated from the tick positions. Use a list to mix the two between
  CI columns, as a vector makes them all double.

- gp:

  Graphical parameters for arrow.

- xlab_gp:

  Graphical parameters for xlab.

- xlim:

  Limits for the x-axis as a vector of length 2, i.e. `c(low, high)`. By
  default the minimum and maximum of the lower and upper values are
  used.

## Value

A grob
