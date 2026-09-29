# Add text to forest plot

This function can be used to add text to a forest plot. The text can
span multiple rows and columns. The height of the row will be adjusted
accordingly if the text is added to only one row. The width of the text
may exceed the columns provided if the text is too long.

## Usage

``` r
add_text(
  plot,
  text,
  row = NULL,
  col = NULL,
  part = c("body", "header"),
  just = c("center", "left", "right"),
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

  Row to add the text, this will be ignored if the `part` is "header".

- col:

  A numeric value or vector indicating the columns the text will be
  added. The text will span over the column if a vector is given.

- part:

  Part to add text, `"body"` (default) or `"header"`.

- just:

  The justification of the text, `"center"` (default), `"left"` or
  `"right"`.

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

[`gtable`](https://gtable.r-lib.org/reference/gtable.html)
[`gpar`](https://rdrr.io/r/grid/gpar.html)
[`textGrob`](https://rdrr.io/r/grid/grid.text.html)
[`gtable_add_grob`](https://gtable.r-lib.org/reference/gtable_add_grob.html)
