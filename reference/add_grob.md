# Add grob in cells

Draw grobs in any cells.

## Usage

``` r
add_grob(
  plot,
  row = NULL,
  col = NULL,
  part = c("body", "header"),
  order = c("top", "text", "background", "bottom"),
  gb_fn,
  ...
)
```

## Arguments

- plot:

  A forest plot object.

- row:

  A numeric value or vector indicating row(s) to draw a grob.

- col:

  A numeric value or vector indicating the columns to draw a grob.

- part:

  The grob will be added to `"body"` (default) or `"header"`.

- order:

  Order in which the grobs should be plotted. Use `'top'` (default) to
  draw the grob above everything, `'text'` on the top of text given by
  plot data but below everything else, `'background'` plot on the top of
  background but below everything else, `'bottom'` below everything.

- gb_fn:

  Grob function

- ...:

  Other parameters to be passed to `gb_fn`.

## Value

A [`gtable`](https://gtable.r-lib.org/reference/gtable.html) object.

## See also

[`gtable_add_grob`](https://gtable.r-lib.org/reference/gtable_add_grob.html)
