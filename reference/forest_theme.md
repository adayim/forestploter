# Forest plot default theme

Default theme for the forest plot. Other parameters can also be passed
and will be forwarded to the corresponding elements of the forest plot.

`forest_theme` is superseded by
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md),
see the section below for how its arguments map onto the new functions.

- `ci_*` Control the graphical parameters of confidence intervals

- `legend_*` Control the graphical parameters of legend

- `xaxis_*` Control the graphical parameters of x-axis

- `refline_*` Control the graphical parameters of reference line

- `vertline_*` Control the graphical parameters of vertical line

- `summary_*` Control the graphical parameters of diamond shaped summary
  CI

- `footnote_*` Control the graphical parameters of footnote

- `title_*` Control the graphical parameters of title

- `arrow_*` Control the graphical parameters of arrow

See [`gpar`](https://rdrr.io/r/grid/gpar.html) for more details.

## Usage

``` r
forest_theme(
  base_size = 12,
  base_family = "",
  ci_pch = 15,
  ci_col = "black",
  ci_alpha = 1,
  ci_fill = NULL,
  ci_lty = 1,
  ci_lwd = 1,
  ci_Theight = NULL,
  legend_name = "Group",
  legend_position = "right",
  legend_value = "",
  legend_gp = gpar(),
  legend_ncol = 1,
  legend_byrow = TRUE,
  xaxis_gp = gpar(),
  refline_gp = gpar(),
  vertline_lwd = 1,
  vertline_lty = "dashed",
  vertline_col = "grey20",
  summary_col = "#4575b4",
  summary_fill = summary_col,
  footnote_gp = gpar(),
  footnote_parse = TRUE,
  title_just = c("left", "right", "center"),
  title_gp = gpar(),
  arrow_type = c("open", "closed"),
  arrow_label_just = c("start", "end"),
  arrow_length = 0.05,
  arrow_gp = gpar(),
  xlab_adjust = c("refline", "center"),
  xlab_gp = gpar(),
  ...
)
```

## Arguments

- base_size:

  The size of text

- base_family:

  The font family

- ci_pch:

  Shape of the point estimation. It will be reused if the forest plot is
  grouped.

- ci_col:

  Color of the CI. A vector of colors should be provided for a grouped
  forest plot. An internal color set will be used if not provided.

- ci_alpha:

  Scalar value, alpha channel for transparency of the point estimate. A
  small vertical line will be added to mark the location of the point
  estimate if this is not equal to 1.

- ci_fill:

  Fill color of the point estimation. A vector of colors should be
  provided for a grouped forest plot. If this is `NULL` (default), the
  value will be inherited from `ci_col`. This is only effective if
  `ci_pch` is within 15:25.

- ci_lty:

  Line type of the CI. A vector of line types should be provided for a
  grouped forest plot.

- ci_lwd:

  Line width of the CI. A vector of line widths should be provided for a
  grouped forest plot.

- ci_Theight:

  A unit specifying the height of the T end of the CI. If set to `NULL`
  (default), no T end will be drawn.

- legend_name:

  Title of the legend.

- legend_position:

  Position of the legend, `"right"`, `"top"`, `"bottom"` or `"none"` to
  suppress the legend.

- legend_value:

  Legend labels (expressions). A vector should be provided for a grouped
  forest plot. Defaults to "Group 1", "Group 2", ... if not provided.

- legend_gp:

  `gpar` graphical parameters of legend, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- legend_ncol:

  integer; the number of columns, see
  [`legendGrob`](https://rdrr.io/r/grid/legendGrob.html).

- legend_byrow:

  logical indicating whether rows of the legend are filled first, see
  [`legendGrob`](https://rdrr.io/r/grid/legendGrob.html).

- xaxis_gp:

  `gpar` graphical parameters of x-axis, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- refline_gp:

  `gpar` graphical parameters of reference line, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- vertline_lwd:

  Line width for extra vertical line. A vector can be provided for each
  vertical line, and the values will be recycled if not enough values
  are given.

- vertline_lty:

  Line type for extra vertical line. Works same as `vertline_lwd`.

- vertline_col:

  Line color for the extra vertical line. Works same as `vertline_lwd`.

- summary_col:

  Color for borders of the summary diamond shape.

- summary_fill:

  Color for filling the summary diamond shape.

- footnote_gp:

  `gpar` graphical parameters of footnote, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- footnote_parse:

  Parse footnote text (default).

- title_just:

  The justification of the title, default is `'left'`.

- title_gp:

  `gpar` graphical parameters of title, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- arrow_type:

  Type of the arrow below x-axis, see
  [`arrow`](https://rdrr.io/r/grid/arrow.html).

- arrow_label_just:

  The justification of the arrow label relative to arrow. Control the
  arrow label to align to the starting point of the arrow `"start"`
  (default) or the ending point of the arrow `"end"`.

- arrow_length:

  The length of the arrow head, default is `0.05`. See
  [`arrow`](https://rdrr.io/r/grid/arrow.html).

- arrow_gp:

  `gpar` graphical parameters of arrow, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- xlab_adjust:

  Control the alignment of xlab to reference line (default) or center of
  the x-axis.

- xlab_gp:

  `gpar` graphical parameters of xlab, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html).

- ...:

  Other parameters passed to table. See
  [`tableGrob`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html) for
  details.

## Value

A list.

## Moving to forest_style()

[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)
takes one [`gpar`](https://rdrr.io/r/grid/gpar.html) for each part of
the plot, and the text of the legend is set with
[`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md).
Themes created with `forest_theme` still work: give them to the `style`
argument of
[`forest`](https://adayim.github.io/forestploter/reference/forest.md) or
to
[`set_style`](https://adayim.github.io/forestploter/reference/set_style.md).

- `base_size`, `base_family`, `ci_pch`, `xlab_adjust`, `title_just`,
  `arrow_type`, `arrow_length`, `arrow_label_just`, `legend_position`,
  `legend_ncol`, `legend_byrow`: the same arguments of `forest_style`.

- `ci_col`, `ci_fill`, `ci_lty`, `ci_lwd`, `ci_alpha`:
  `forest_style(ci = gpar(col, fill, lty, lwd, alpha))`.

- `ci_Theight`: `forest_style(ci_t_height)`.

- `summary_col`, `summary_fill`:
  `forest_style(summary = gpar(col, fill))`.

- `vertline_lwd`, `vertline_lty`, `vertline_col`:
  `forest_style(vline = gpar(lwd, lty, col))`.

- `refline_gp`: `forest_style(ref_line)`.

- `xaxis_gp`, `xlab_gp`, `title_gp`, `footnote_gp`, `arrow_gp`,
  `legend_gp`: the arguments of `forest_style` without `_gp`, e.g.
  `forest_style(title = gpar(col = "red"))`.

- `footnote_parse`: `forest_style(parse)`, which also applies to the
  title, x-axis labels, arrow labels and legend labels.

- `legend_name`, `legend_value`: `legend_title` and `legend_labels` of
  [`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md).

- The fill of `core` and `colhead`, e.g.
  `core = list(bg_params = list(fill = "white"))`:
  `forest_style(body = gpar(fill = "white"))` and
  `forest_style(header = gpar(fill = "white"))`.

- Other table settings in `...`: passed to `...` of `forest_style` in
  the same way.

## See also

[`tableGrob`](https://rdrr.io/pkg/gridExtra/man/tableGrob.html)
[`forest`](https://adayim.github.io/forestploter/reference/forest.md)
[`textGrob`](https://rdrr.io/r/grid/grid.text.html)
[`gpar`](https://rdrr.io/r/grid/gpar.html)
[`arrow`](https://rdrr.io/r/grid/arrow.html)
[`segmentsGrob`](https://rdrr.io/r/grid/grid.segments.html)
[`linesGrob`](https://rdrr.io/r/grid/grid.lines.html)
[`pointsGrob`](https://rdrr.io/r/grid/grid.points.html)
[`legendGrob`](https://rdrr.io/r/grid/legendGrob.html)
