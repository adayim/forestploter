# Insert text to forest plot

This function can be used to insert text into a forest plot. Remember to
adjust for the row number if you have added text before, including the
header. This is achieved by inserting new row(s) into the plot and will
affect subsequent row numbers. A text vector can be inserted into
multiple columns or rows.

## Usage

``` r
insert_text(
  plot,
  text,
  row = NULL,
  col = NULL,
  part = c("body", "header"),
  just = c("center", "left", "right"),
  before = TRUE,
  gp = gpar(),
  padding = unit(1, "mm"),
  parse = FALSE
)
```

## Arguments

- plot:

  A forest plot object.

- text:

  A character or expression vector, see
  [`textGrob`](https://rdrr.io/r/grid/grid.text.html).

- row:

  Row to insert the text, this will be ignored if the `part` is
  "header".

- col:

  A numeric value or vector indicating the columns the text will be
  added. The text will span over the column if a vector is given.

- part:

  Part to insert text, `"body"` (default) or `"header"`.

- just:

  The justification of the text, `"center"` (default), `"left"` or
  `"right"`.

- before:

  Indicating the text will be inserted before or after the row.

- gp:

  An object of class `"gpar"`, this is the graphical parameter settings
  of the text. See [`gpar`](https://rdrr.io/r/grid/gpar.html).

- padding:

  Padding of the text, default is `unit(1, "mm")`

- parse:

  Logical, behaviour for parsing text as plotmath, see
  [`plotmath`](https://rdrr.io/r/grDevices/plotmath.html)

## Value

A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

## See also

[`gpar`](https://rdrr.io/r/grid/gpar.html)
[`textGrob`](https://rdrr.io/r/grid/grid.text.html)
[`gtable_add_grob`](https://gtable.r-lib.org/reference/gtable_add_grob.html)
