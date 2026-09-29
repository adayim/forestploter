# Create legends

This function used to create legends for the forest plot.

## Usage

``` r
legend_grob(
  name = "",
  label,
  position = c("right", "top", "bottom"),
  hgap = unit(0.1, "lines"),
  vgap = unit(0.5, "lines"),
  pch = 15,
  ncol = 1,
  gp = gpar(lty = 1, col = "black", fill = "black", fontsize = 12, fontfamily = ""),
  byrow = TRUE,
  ...
)
```

## Arguments

- name:

  Character string, Legend name.

- label:

  legend labels (expressions).

- position:

  Position of the legend, `"right"`, `"top"`, `"bottom"`.

- hgap:

  Horizontal gap between the legend entries, see
  [`legendGrob`](https://rdrr.io/r/grid/legendGrob.html) for details.

- vgap:

  Vertical gap between the legend entries, see
  [`legendGrob`](https://rdrr.io/r/grid/legendGrob.html) for details.

- pch:

  Legend symbol.

- ncol:

  integer; the number of columns

- gp:

  Graphical parameters.

- byrow:

  logical indicating whether rows of the legend are filled first.

- ...:

  Other parameters, not used currently.

## Value

A frame grob
