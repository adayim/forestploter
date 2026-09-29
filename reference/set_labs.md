# Set the labels of a forest plot

Set the title, x-axis labels, footnote, arrow labels and legend text of
a forest plot. Their look, including the justification of the title, the
arrow type and the legend position, is set with
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md).

## Usage

``` r
set_labs(plot, title, xlab, footnote, arrow, legend_title, legend_labels)
```

## Arguments

- plot:

  A forest plot object, see
  [`forest`](https://adayim.github.io/forestploter/reference/forest.md).

- title:

  The text for the title.

- xlab:

  X-axis labels, put under the x-axis. A vector with one label for each
  CI column gives different labels to the columns, `NA` leaves a column
  without a label.

- footnote:

  Footnote for the forest plot, aligned at the left bottom of the plot.
  Please adjust the line length with line breaks to avoid overlap with
  the arrows and/or x-axis.

- arrow:

  Labels for the arrows under the x-axis, a vector of length two (left
  and right). A list with one pair of labels for each CI column gives
  different arrows to the columns, `NA` leaves a column without arrows.

- legend_title:

  Title of the legend of a grouped forest plot, the default is "Group".

- legend_labels:

  Legend labels, one for each group. Defaults to "Group 1", "Group 2",
  ...

## Value

A forest plot object.

## Details

Arguments left out keep their current value, so the labels can be set in
several calls. `NULL` removes a label, or for the legend goes back to
the default text.

This builds the plot again, so it must be used before the plot is edited
with
[`edit_plot`](https://adayim.github.io/forestploter/reference/edit_plot.md),
[`add_text`](https://adayim.github.io/forestploter/reference/add_text.md),
[`insert_text`](https://adayim.github.io/forestploter/reference/insert_text.md),
[`add_border`](https://adayim.github.io/forestploter/reference/add_border.md)
or
[`add_grob`](https://adayim.github.io/forestploter/reference/add_grob.md).

## See also

[`forest`](https://adayim.github.io/forestploter/reference/forest.md)
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)

## Examples

``` r
library(grid)
# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:6, ]

# Add a blank column for the forest plot to display CI
dt$` ` <- paste(rep(" ", 20), collapse = " ")

p <- forest(dt[, c("Subgroup", " ")],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            ci_column = 2,
            ref_line = 1)

p <- set_labs(p,
              title = "Subgroup analysis",
              xlab = "Hazard ratio",
              arrow = c("Placebo Better", "Treatment Better"),
              footnote = "This is the demo data.")
plot(p)


# Labels left out are kept, `NULL` removes a label
plot(set_labs(p, footnote = NULL))
```
