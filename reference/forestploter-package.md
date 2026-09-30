# forestploter: create a flexible forest plot

The layout of the forest plot is the layout of the data given to
[`forest`](https://adayim.github.io/forestploter/reference/forest.md),
which draws the table and the confidence intervals. The other parts are
added with functions that take the plot as their first argument, so they
can be chained with the pipe `|>`:

## Details

- [`set_xaxis`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
  Limits, tick marks and scale of the x-axis, and vertical lines

- [`set_labs`](https://adayim.github.io/forestploter/reference/set_labs.md)
  Title, x-axis labels, footnote, arrow labels and legend labels

- [`scale_sizes`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  Point sizes scaled by study weights

- [`set_style`](https://adayim.github.io/forestploter/reference/set_style.md)
  Graphical parameters, see
  [`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)

The plot is a [`gtable`](https://gtable.r-lib.org/reference/gtable.html)
at every step, so it can be drawn, saved with `ggplot2::ggsave` or
combined with other plots. Afterwards it can be edited cell by cell with
[`edit_plot`](https://adayim.github.io/forestploter/reference/edit_plot.md),
[`add_text`](https://adayim.github.io/forestploter/reference/add_text.md),
[`insert_text`](https://adayim.github.io/forestploter/reference/insert_text.md),
[`add_border`](https://adayim.github.io/forestploter/reference/add_border.md)
and
[`add_grob`](https://adayim.github.io/forestploter/reference/add_grob.md).

The vignettes
[`vignette("forestploter-intro")`](https://adayim.github.io/forestploter/articles/forestploter-intro.md)
and
[`vignette("forestploter-post")`](https://adayim.github.io/forestploter/articles/forestploter-post.md)
walk through both steps.

## See also

Useful links:

- <https://github.com/adayim/forestploter>

- <https://adayim.github.io/forestploter/>

- Report bugs at <https://github.com/adayim/forestploter/issues>

## Author

**Maintainer**: Alimu Dayimu <ad938@cam.ac.uk>
([ORCID](https://orcid.org/0000-0001-9998-7463))
