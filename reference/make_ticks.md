# Set x-axis ticks

Pick tick positions in the (already-transformed) `xlim` space.

## Usage

``` r
make_ticks(at = NULL, xlim, refline = 1, x_trans = "none")
```

## Arguments

- at:

  Numerical vector, create ticks at given values.

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

## Value

A vector of tick coordinates in the transformed space.

## Details

For `x_trans` in `"none"` / `"scientific"` this delegates to
[`pretty`](https://rdrr.io/r/base/pretty.html). For `"log"` / `"log2"` /
`"log10"` it converts `xlim` back to the original scale, runs
[`log_pretty`](https://adayim.github.io/forestploter/reference/log_pretty.md)
to get base-aware ticks (e.g. `0.1, 1, 10` rather than
`0.22, 0.61, 2.72`), and re-applies the transform. The reference line
value is included as a tick when it falls inside the range, so forest
plots always label their visual anchor.
