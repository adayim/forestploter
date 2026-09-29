# Set the x-axis

Set the limits, tick marks and scale of the x-axis of the CI columns,
and add vertical lines to them.

## Usage

``` r
set_xaxis(plot, xlim, ticks_at, ticks_digits, ticks_minor, x_trans, vline)
```

## Arguments

- plot:

  A forest plot object, see
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md).

- xlim:

  Limits for the x-axis as a vector of length 2, i.e. `c(low, high)`. By
  default the minimum and maximum of the lower and upper values are
  used.

- ticks_at:

  Tick mark positions. By default, ticks are computed automatically:
  [`pretty`](https://rdrr.io/r/base/pretty.html) for linear axes and a
  decade-aware helper for log scales (e.g. `0.1, 1, 10, 100` for a wide
  log10 range).

- ticks_digits:

  Number of digits for the tick labels. If an integer is given, for
  example `1L`, trailing zeros after the decimal mark are dropped. Give
  a double, for example `1`, to keep them. By default the number is
  calculated from the tick positions. Use a list to mix the two between
  CI columns, as a vector makes them all double.

- ticks_minor:

  A numeric vector of positions to draw ticks without labels. It can be
  a superset of `ticks_at` or disjoint from it.

- x_trans:

  Scale of the axis, one of `"none"` (default), `"log"`, `"log2"` or
  `"log10"`. Use `"log"` if the values are exponential, e.g. odds ratios
  or hazard ratios. The default reference line of
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md)
  is 1 for log scales and 0 otherwise.

- vline:

  Numeric vector, positions of vertical lines drawn in addition to the
  reference line, on the original scale of the x-axis. Their look is set
  with `vertline` of
  [`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md).
  No lines are drawn by default.

## Value

A forest plot object.

## Details

Arguments left out keep their current value, so the axis can be set in
several calls, and `NULL` goes back to the default. A single value
applies to all CI columns. To give the columns different settings,
provide a list with one element for each CI column (a vector for
`ticks_digits` and `x_trans`), where `NA` leaves a column at its
default.

This builds the plot again, so it must be used before the plot is edited
with
[`edit_plot`](https://adayim.github.io/forestploter/reference/edit_plot.md),
[`add_text`](https://adayim.github.io/forestploter/reference/add_text.md),
[`insert_text`](https://adayim.github.io/forestploter/reference/insert_text.md),
[`add_border`](https://adayim.github.io/forestploter/reference/add_border.md)
or
[`add_grob`](https://adayim.github.io/forestploter/reference/add_grob.md).

## See also

[`forest`](https://adayim.github.io/forestploter/reference/forest.md)
[`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md)
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)

## Examples

``` r
library(grid)
# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:6, ]

# Add a blank column for the forest plot to display CI
dt$` ` <- paste(rep(" ", 20), collapse = " ")

p <- forest(dt[, c("Subgroup", " ")],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            ci_column = 2,
            ref_line = 1)

# Limits and tick marks, with a vertical line at 2
p <- set_xaxis(p, xlim = c(0, 4), ticks_at = c(0.5, 1, 2, 3), vline = 2)
plot(p)


# The axis can be set in several calls, `NULL` goes back to the default
p <- set_xaxis(p, ticks_digits = 1L)
p <- set_xaxis(p, vline = NULL)

# A log scale, where the reference line of `forest()` defaults to 1
plot(set_xaxis(p, x_trans = "log", xlim = c(0.25, 4), ticks_at = c(0.5, 1, 2, 4)))
```
