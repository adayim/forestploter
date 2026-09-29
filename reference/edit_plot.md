# Edit forest plot

This function is used to edit the graphical parameters of text and
background of the forest plot.

## Usage

``` r
edit_plot(
  plot,
  row = NULL,
  col = NULL,
  part = c("body", "header"),
  which = c("text", "background", "ci"),
  gp = gpar(),
  ...
)
```

## Arguments

- plot:

  A forest plot object.

- row:

  A numeric value or vector indicating row number to edit in the
  dataset. Will edit the whole row if left blank for the body. This will
  be ignored if the `part` is "header".

- col:

  A numeric value or vector indicating column to edit in the dataset.
  Will edit the whole column if left blank.

- part:

  Part to edit, `"body"` (default) or `"header"`.

- which:

  Which element to edit, `"text"`, `"background"` or `"ci"` (confidence
  interval). This will not edit diamond shaped summary CI, please change
  it with
  [`forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md).
  Also, change in `ci` will not have any impact on the legend.

- gp:

  Pass `gpar` parameters, see
  [`gpar`](https://rdrr.io/r/grid/gpar.html). It should be passed as
  `gpar(col = "red")`. For `which = "ci"`, please refer to
  [`forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  `ci_*` parameters for the editable elements.

- ...:

  Other parameters to be passed to the grobs. See
  [`textGrob`](https://rdrr.io/r/grid/grid.text.html) for the `"text"`
  part and [`rectGrob`](https://rdrr.io/r/grid/grid.rect.html) for
  `"background"`. This is ignored when `which = "ci"` because
  non-graphical parameters cannot be changed for the confidence
  interval.

## Value

A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

## See also

[`gpar`](https://rdrr.io/r/grid/gpar.html)
[`editGrob`](https://rdrr.io/r/grid/grid.edit.html)
[`forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
[`textGrob`](https://rdrr.io/r/grid/grid.text.html)
[`rectGrob`](https://rdrr.io/r/grid/grid.rect.html)
