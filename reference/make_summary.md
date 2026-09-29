# Create pooled summary diamond shape

Create pooled summary diamond shape

## Usage

``` r
make_summary(est, lower, upper, sizes = 1, gp, xlim, nudge_y = 0)
```

## Arguments

- est:

  Point estimation. Can be a list for multiple columns and/or multiple
  groups. If the length of the list is larger than then length of
  `ci_column`, then the values reused for each column and considered as
  different groups.

- lower:

  Lower bound of the confidence interval, same as `est`.

- upper:

  Upper bound of the confidence interval, same as `est`.

- sizes:

  Size of the point estimation box, can be a vector or a list. The value
  is a multiple of one line of text, so `1` draws a point as tall as the
  `base_size` of the theme. The same scale applies to the summary
  diamond. Values are used as they are, unless
  [`scale_sizes`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  is used to read them as study weights; useful values are roughly
  between `0.2` and `1.5`, and a warning is given when the plot is drawn
  if they are outside `0.1` to `2`.

- gp:

  Graphical parameters of [`gpar`](https://rdrr.io/r/grid/gpar.html).
  Please refer to
  [`forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  for more details.

- xlim:

  Limits for the x-axis as a vector of length 2, i.e. `c(low, high)`. By
  default the minimum and maximum of the lower and upper values are
  used.

- nudge_y:

  Vertical adjustment to nudge groups by, must be within 0 to 1.
  Defaults to `0`; for grouped forest plots a value of `0` is bumped to
  `0.1` automatically so that group CIs do not overplot. Set explicitly
  to override.

## Value

A gTree object
