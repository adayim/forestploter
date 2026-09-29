# Checking error for forest plot

The settings of the x-axis and the labels are checked by
[`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
and
[`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md).

## Usage

``` r
check_errors(data, est, lower, upper, sizes, ref_line, ci_column, is_summary)
```

## Arguments

- data:

  Data to be displayed in the forest plot

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

- ref_line:

  X-axis coordinates of the reference line, the value of no effect. If
  `NULL` (default), it is 1 if the x-axis is on a log scale (see
  [`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md))
  and 0 otherwise. Provide an atomic vector if different reference line
  for each `ci_column` is desired.

- ci_column:

  Column number of the data the CI will be displayed.

- is_summary:

  A logical vector indicating if the value is a summary value, which
  will have a diamond shape for the estimate. With multiple groups the
  diamonds are stacked in the same cell and the summary rows are made
  taller to fit them, so a larger `nudge_y` may be wanted.
