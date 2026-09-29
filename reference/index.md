# Package index

## Create a forest plot

The data gives the layout of the plot, `forest` draws the table and the
confidence intervals.

- [`forest()`](https://adayim.github.io/forestploter/reference/forest.md)
  : Forest plot

## Add to the plot

These take the plot as their first argument, so they can be chained with
the pipe.

- [`set_xaxis()`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
  : Set the x-axis
- [`set_labs()`](https://adayim.github.io/forestploter/reference/set_labs.md)
  : Set the labels of a forest plot
- [`scale_sizes()`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  : Scale point sizes by weights
- [`set_style()`](https://adayim.github.io/forestploter/reference/set_style.md)
  : Set the style of a forest plot
- [`forest_style()`](https://adayim.github.io/forestploter/reference/forest_style.md)
  : Forest plot style

## Edit the plot

Change the cells of a plot that is already built, by row and column.

- [`edit_plot()`](https://adayim.github.io/forestploter/reference/edit_plot.md)
  : Edit forest plot
- [`add_text()`](https://adayim.github.io/forestploter/reference/add_text.md)
  : Add text to forest plot
- [`insert_text()`](https://adayim.github.io/forestploter/reference/insert_text.md)
  : Insert text to forest plot
- [`add_border()`](https://adayim.github.io/forestploter/reference/add_border.md)
  : Add border to cells
- [`add_grob()`](https://adayim.github.io/forestploter/reference/add_grob.md)
  : Add grob in cells

## Draw the confidence intervals

The functions given to `fn_ci` and `fn_summary` of `forest`, and an
example of writing your own.

- [`makeci()`](https://adayim.github.io/forestploter/reference/makeci.md)
  : Create confidence interval grob
- [`make_summary()`](https://adayim.github.io/forestploter/reference/make_summary.md)
  : Create pooled summary diamond shape
- [`make_boxplot()`](https://adayim.github.io/forestploter/reference/make_boxplot.md)
  : Create horizontal boxplot grob

## Draw and save

- [`print(`*`<forestplot>`*`)`](https://adayim.github.io/forestploter/reference/print.forestplot.md)
  [`plot(`*`<forestplot>`*`)`](https://adayim.github.io/forestploter/reference/print.forestplot.md)
  : Draw plot
- [`print(`*`<forest_style>`*`)`](https://adayim.github.io/forestploter/reference/print.forest_style.md)
  : Print a forest plot style
- [`get_wh()`](https://adayim.github.io/forestploter/reference/get_wh.md)
  : Get width and height of the forestplot

## Superseded

Kept so that code written before version 1.2.0 keeps working, see
[`?forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
for the settings of `forest_style`.

- [`forest_theme()`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  : Forest plot default theme
