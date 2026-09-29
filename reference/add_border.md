# Add border to cells

Add border to any cells at any side.

## Usage

``` r
add_border(
  plot,
  row = NULL,
  col = NULL,
  part = c("body", "header"),
  where = c("bottom", "left", "top", "right"),
  gp = gpar(lwd = 2)
)
```

## Arguments

- plot:

  A forest plot object.

- row:

  A numeric value or vector indicating row number to add border. This is
  corresponding to the data row number. Remember to account for any text
  inserted. A border will be drawn to all rows if this is omitted.

- col:

  A numeric value or vector indicating the columns to add border. A
  border will be drawn to all columns if this is omitted.

- part:

  The border will be added to `"body"` (default) or `"header"`.

- where:

  Where to draw the border of the cell, possible values are `"bottom"`
  (default), `"left"`, `"top"` and `"right"`

- gp:

  An object of class `"gpar"`, graphical parameter to be passed to
  [`segmentsGrob`](https://rdrr.io/r/grid/grid.segments.html).

## Value

A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

## See also

[`gpar`](https://rdrr.io/r/grid/gpar.html)
[`segmentsGrob`](https://rdrr.io/r/grid/grid.segments.html)
[`gtable_add_grob`](https://gtable.r-lib.org/reference/gtable_add_grob.html)
