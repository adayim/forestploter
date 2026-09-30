# Changelog

## forestploter 1.2.0

### Breaking changes

- `sizes` is now a multiple of one line of text for both points and
  summary diamonds, where it used to be `char` for one and a fraction of
  the row height for the other. **Summary diamonds are flatter than in
  1.1.4**; pass a larger `sizes` to restore the old look.
- Point size now follows the theme’s `base_size` instead of the
  pointsize of whichever device happened to be open. Unchanged at the
  default `base_size`.
- Summary rows in grouped plots grow to the height the group offsets
  need, rather than always doubling.

### New features

- The plot is now built step by step with the pipe `|>`.
  [`forest()`](https://adayim.github.io/forestploter/reference/forest.md)
  draws the table and the confidence intervals, and the rest is added
  with
  [`set_xaxis()`](https://adayim.github.io/forestploter/reference/set_xaxis.md)
  (limits, tick marks, scale and vertical lines),
  [`set_labs()`](https://adayim.github.io/forestploter/reference/set_labs.md)
  (title, x-axis labels, footnote, arrow labels and legend text),
  [`scale_sizes()`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  (point sizes from study weights) and
  [`set_style()`](https://adayim.github.io/forestploter/reference/set_style.md).
  They must be used before the plot is edited with
  [`edit_plot()`](https://adayim.github.io/forestploter/reference/edit_plot.md)
  and the other editing functions.
- Graphical parameters are set with
  [`forest_style()`](https://adayim.github.io/forestploter/reference/forest_style.md),
  which takes one [`gpar()`](https://rdrr.io/r/grid/gpar.html) for each
  part of the plot and is given to the new `style` argument of
  [`forest()`](https://adayim.github.io/forestploter/reference/forest.md).
  [`forest_theme()`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  is superseded but keeps working, also with
  [`set_style()`](https://adayim.github.io/forestploter/reference/set_style.md);
  [`?forest_theme`](https://adayim.github.io/forestploter/reference/forest_theme.md)
  shows how its arguments map onto
  [`forest_style()`](https://adayim.github.io/forestploter/reference/forest_style.md).
- New `fit` of
  [`forest_style()`](https://adayim.github.io/forestploter/reference/forest_style.md)
  lets the plot use the space it is drawn in: `"width"` gives the free
  width to the CI columns and `"both"` also shares the free height
  between the rows. The default `"none"` keeps the natural size of the
  plot, as before. The `autofit` argument of
  [`print()`](https://rdrr.io/r/base/print.html) will be deprecated in
  favour of it.
- New
  [`scale_sizes()`](https://adayim.github.io/forestploter/reference/scale_sizes.md)
  to scale study weights into point sizes, following `metafor` and
  `meta`. Without it, `sizes` are used as they are
  ([\#37](https://github.com/adayim/forestploter/issues/37)).
- A warning is given when the plot is drawn if `sizes` falls outside 0.1
  to 2, or if grouped confidence intervals are likely to overlap given
  `nudge_y`.
- In the new functions, an argument left out keeps its current value and
  `NULL` goes back to the default. `NA` is only used for a CI column
  left at its default, as in `xlim = list(c(0, 4), NA)`.

### Superseded

- The arguments of
  [`forest()`](https://adayim.github.io/forestploter/reference/forest.md)
  that the functions above replace are still accepted and draw the same
  plot. Each of them gives a message once per session pointing to its
  replacement: `xlim`, `ticks_at`, `ticks_digits`, `ticks_minor`,
  `x_trans` and `vert_line` to
  [`set_xaxis()`](https://adayim.github.io/forestploter/reference/set_xaxis.md),
  and `arrow_lab`, `xlab`, `title` and `footnote` to
  [`set_labs()`](https://adayim.github.io/forestploter/reference/set_labs.md),
  and `theme` to `style`. They will be removed in 2.0.0.
- [`forest()`](https://adayim.github.io/forestploter/reference/forest.md)
  now gives a message once per session for an argument in `...` that
  neither `fn_ci`, `fn_summary` nor `index_args` takes, instead of
  dropping it silently. The argument is still ignored.
- The package now requires R \>= 4.1.0 for the native pipe used in the
  examples.

### Bug fixes

- Fix automatic `ticks_digits` dropping decimals on linear axes, which
  rendered fractional ticks with duplicated labels (e.g. `1, 1.5, 2` as
  “1”, “2”, “2”).
- Fix error when `gp` is passed to `forest` with summary rows.
- Error if the length of `est` is not a multiple of the length of
  `ci_column`, instead of silently dropping the extra series.
- Replace `gridtext` with `gridmicrotex` in the vignettes, so the
  annotation examples are typeset as real LaTeX math.

## forestploter 1.1.4

CRAN release: 2026-04-27

- Deprecated some parameters in `forest_theme`.
- Remove gap between cells.
- Better ticks break.
- Code base improvement.
- Fix typos.

## forestploter 1.1.3

CRAN release: 2025-04-13

- Extend vertical line to the top and the bottom.
- Allow multiple lines of legend with `legend_ncol` in `forest_theme`.
- Allow control legend fills with `legend_byrow` in `forest_theme`.
- Fix error in vignette.

## forestploter 1.1.2

CRAN release: 2024-04-13

- Draw reference line and other vertical lines below whiskers.
- Allow minor ticks and groups for diamond shapes.
- Able to change legend size.
- Able to change all graphical parameters of title, legends, x-axis,
  arrow labs, footnote and reference line.

## forestploter 1.1.1

CRAN release: 2023-09-23

- Improved `ticks_digits` auto calculation.
- Remove self righteousness cell height adjustment.
- Able to change the fontsize and alignment of the `xlab`.
- Miss seplled `backgroud` parameter in `add_grob`.

## forestploter 1.1.0

CRAN release: 2023-04-11

- New function `make_boxplot` to draw boxplot inside the plot.
- `forest` now accepts custom CI functions and Summary functions.

## forestploter 1.0.0

CRAN release: 2023-02-08

- New function `add_grob`.
- `add_text` and `insert_text` can parse math symbol.
- `edit_plot` accepts more paramters.
- Point size in the forestplot will no longer be transformed.
- Summary fill will inherit the summary color in the `forest_theme`
  function.
- Better vignettes.
- Fix an issue in with ticks digits in `forest`.

## forestploter 0.2.3

CRAN release: 2022-11-20

- Fixed a bug of legend point estimation color not changing.
- There’s a new function `add_border` to add border to any cell at any
  side.
- Digits rounding now respect `ticks_digits`.

## forestploter 0.2.2

CRAN release: 2022-10-19

- Fixed a bug of not drawing groups larger than 3.
- Able to change the color of the point estimation.
- Able to change the transparency of the point estimation.

## forestploter 0.2.1

CRAN release: 2022-09-29

- Fixed bug of point estimation not showing.
- Able to change the color of the CI now.

## forestploter 0.2.0

CRAN release: 2022-09-20

- Improve calculation of `szies`.

## forestploter 0.1.9

CRAN release: 2022-08-29

- Fix bugs in `szies`.

## forestploter 0.1.8

CRAN release: 2022-08-28

- Added options for arrows for alignment and other controls.
- Added `x_trans` options for different scales of x-axis.
- Added `get_wh` and `get_scale` for saving plots.
- `xlog` has been deprecated, should be define in `x_trans`.

## forestploter 0.1.7

CRAN release: 2022-08-07

- Fixed minor issue in inserting text.
- Able to suppress legend

## forestploter 0.1.6

CRAN release: 2022-06-23

- Able to define the rounding digits for ticks.
- Able to add title to the plot.

## forestploter 0.1.5

CRAN release: 2022-05-07

- Fixed issues with checks for zeros if `xlog=TRUE`.
- Different reference line, x-axis label, xlog, vertical line, xlim,
  x-axis ticks and arrow label is possible for different CI columns.
- Able to add x-axis label.

## forestploter 0.1.4

CRAN release: 2022-03-21

- Fixed issues with xlim calculation.
- Fixed bug in theme setting axis cex not used.
- Added some unit tests.

## forestploter 0.1.3

CRAN release: 2022-03-13

- Added CI line width option.
- Added CI T end option.
- Fixed bug in `xlim`.

## forestploter 0.1.2

CRAN release: 2022-03-01

- Added `xlog` options for exponential estimates, eg HR, OR.
- Auto calculate x-ticks and xlim for multiple column.
- Minor updates and changes.

## forestploter 0.1.1

CRAN release: 2022-01-20

- Added summary diamond shape.
- Able to change CI line type.
- Able to insert text vector to multiple column.
- Able to select row header for plot editing.
- Print plot with auto fit.
- Fixed row calculation in plot editing.
- Fixed some typos.

## forestploter 0.0.1

CRAN release: 2022-01-11

- Initial release.
