# Create xlim

Create xlim based on value ranges.

## Usage

``` r
make_xlim(
  xlim = NULL,
  lower,
  upper,
  ref_line = ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0),
  ticks_at = NULL,
  x_trans = "none"
)
```

## Arguments

- xlim:

  Limits for the x-axis as a vector of length 2, i.e. `c(low, high)`. By
  default the minimum and maximum of the lower and upper values are
  used.

- lower:

  Lower bound of the confidence interval, same as `est`.

- upper:

  Upper bound of the confidence interval, same as `est`.

- ref_line:

  X-axis coordinates of the reference line, the value of no effect. If
  `NULL` (default), it is 1 if the x-axis is on a log scale (see
  [`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md))
  and 0 otherwise. Provide an atomic vector if different reference line
  for each `ci_column` is desired.

- ticks_at:

  Tick mark positions. By default, ticks are computed automatically:
  [`pretty`](https://rdrr.io/r/base/pretty.html) for linear axes and a
  decade-aware helper for log scales (e.g. `0.1, 1, 10, 100` for a wide
  log10 range).

- x_trans:

  Scale of the axis, one of `"none"` (default), `"log"`, `"log2"` or
  `"log10"`. Use `"log"` if the values are exponential, e.g. odds ratios
  or hazard ratios. The default reference line of
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md)
  is 1 for log scales and 0 otherwise.

## Value

A list
