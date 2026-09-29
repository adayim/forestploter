# Forest plot

A data frame will be used for the basic layout of the forest plot.
Graphical parameters can be set using the
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)
function.

`forest` draws the table and the confidence intervals. The other parts
of the plot are added with functions that take the plot as their first
argument, so they can be chained with the pipe `|>`:

- [`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
  Limits, tick marks and scale of the x-axis, and vertical lines

- [`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md)
  Title, x-axis labels, footnote, arrow labels and legend labels

- [`scale_sizes`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  Point sizes scaled by study weights

- [`set_style`](https://adayim.github.io/forestploter/reference/set_style.md)
  Graphical parameters

These functions build the plot again, so they must be used before the
plot is edited with
[`edit_plot`](https://adayim.github.io/forestploter/reference/edit_plot.md),
[`add_text`](https://adayim.github.io/forestploter/reference/add_text.md),
[`insert_text`](https://adayim.github.io/forestploter/reference/insert_text.md),
[`add_border`](https://adayim.github.io/forestploter/reference/add_border.md)
or
[`add_grob`](https://adayim.github.io/forestploter/reference/add_grob.md).
The plot stays a
[`gtable`](https://gtable.r-lib.org/reference/gtable.html) at every
step, and can be combined with other plots, e.g. with
`patchwork::wrap_elements`.

## Usage

``` r
forest(
  data,
  est,
  lower,
  upper,
  sizes = 0.4,
  ref_line = NULL,
  ci_column,
  is_summary = NULL,
  nudge_y = 0,
  fn_ci = makeci,
  fn_summary = make_summary,
  index_args = NULL,
  style = NULL,
  ...
)
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

- nudge_y:

  Vertical adjustment to nudge groups by, must be within 0 to 1.
  Defaults to `0`; for grouped forest plots a value of `0` is bumped to
  `0.1` automatically so that group CIs do not overplot. Set explicitly
  to override.

- fn_ci:

  Name of the function to draw confidence interval, default is
  [`makeci`](https://adayim.github.io/forestploter/reference/makeci.md).
  You can specify your own drawing function to draw the confidence
  interval, but the function needs to accept arguments
  ` "est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y"`.
  Please refer to the
  [`makeci`](https://adayim.github.io/forestploter/reference/makeci.md)
  function for the details of these parameters.

- fn_summary:

  Name of the function to draw summary confidence interval, default is
  [`make_summary`](https://adayim.github.io/forestploter/reference/make_summary.md).
  You can specify your own drawing function to draw the summary
  confidence interval, but the function needs to accept arguments
  `"est", "lower", "upper", "sizes", "xlim", "gp"`. Please refer to the
  [`make_summary`](https://adayim.github.io/forestploter/reference/make_summary.md)
  function for the details of these parameters.

- index_args:

  A character vector, name of the arguments used for indexing the row
  and column. This should be the name of the arguments that is working
  the same way as `est`, `lower` and `upper`. Check out the examples in
  the
  [`make_boxplot`](https://adayim.github.io/forestploter/reference/make_boxplot.md).

- style:

  Style of the forest plot created with
  [`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md).
  A theme created with the superseded
  [`forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  is also accepted. The style can also be set or changed later with
  [`set_style`](https://adayim.github.io/forestploter/reference/set_style.md).

- ...:

  Other arguments passed on to the `fn_ci` and `fn_summary`, or named in
  `index_args`. An argument none of them takes gives an error, as it
  would not be used. The arguments of earlier versions are also accepted
  here, with a message the first time each of them is used in a session:
  use
  [`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
  instead of `xlim`, `ticks_at`, `ticks_digits`, `ticks_minor`,
  `x_trans` and `vert_line`,
  [`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md)
  instead of `arrow_lab`, `xlab`, `title` and `footnote`, and `style`
  instead of `theme`.

## Value

A forest plot object, a
[`gtable`](https://gtable.r-lib.org/reference/gtable.html) of class
`forestplot`.

## See also

[`gtable`](https://gtable.r-lib.org/reference/gtable.html)
[`tableGrob`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html)
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)
[`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
[`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md)
[`scale_sizes`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
[`set_style`](https://adayim.github.io/forestploter/reference/set_style.md)
[`make_boxplot`](https://adayim.github.io/forestploter/reference/make_boxplot.md)
[`makeci`](https://adayim.github.io/forestploter/reference/makeci.md)
[`make_summary`](https://adayim.github.io/forestploter/reference/make_summary.md)

## Examples

``` r
library(grid)
# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# Keep needed columns
dt <- dt[,1:6]

# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo),
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space.
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$est, dt$low, dt$hi))

# Define a style
st <- forest_style(base_size = 10,
                   ref_line = gpar(col = "red"),
                   footnote = gpar(col = "#636363", fontface = "italic"))

# Draw the plot and add the axis and labels with a pipe
p <- forest(dt[,c(1:3, 8:9)],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            sizes = dt$se,
            ci_column = 4,
            ref_line = 1,
            style = st) |>
  set_xaxis(xlim = c(0, 4), ticks_at = c(0.5, 1, 2, 3)) |>
  set_labs(arrow = c("Placebo Better", "Treatment Better"),
           footnote = "This is the demo data. Please feel free to change\nanything you want.")

# Print plot
plot(p)

```
