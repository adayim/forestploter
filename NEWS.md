# forestploter 1.1.2

* Draw reference line and other vertical lines below whiskers.
* Allow minor ticks and groups for diamond shapes.
* Able to change legend size.
* Able to change all graphical parameters of title, legends, x-axis, arrow labs, footnote and reference line.

# forestploter 1.1.1

* Improved `ticks_digits` auto calculation.
* Remove self righteousness cell height adjustment. 
* Able to change the fontsize and alignment of the `xlab`.
* Miss seplled `backgroud` parameter in `add_grob`.

# forestploter 1.1.0

* New function `make_boxplot` to draw boxplot inside the plot.
* `forest` now accepts custom CI functions and Summary functions.

# forestploter 1.0.0

* New function `add_grob`.
* `add_text` and `insert_text` can parse math symbol. 
* `edit_plot` accepts more paramters.
* Point size in the forestplot will no longer be transformed.
* Summary fill will inherit the summary color in the `forest_theme` function.
* Better vignettes.
* Fix an issue in with ticks digits in `forest`.

# forestploter 0.2.3

* Fixed a bug of legend point estimation color not changing.
* There's a new function `add_border` to add border to any cell at any side.
* Digits rounding now respect `ticks_digits`.

# forestploter 0.2.2

* Fixed a bug of not drawing groups larger than 3.
* Able to change the color of the point estimation.
* Able to change the transparency of the point estimation.

# forestploter 0.2.1

* Fixed bug of point estimation not showing.
* Able to change the color of the CI now.

# forestploter 0.2.0

* Improve calculation of `szies`.

# forestploter 0.1.9

* Fix bugs in `szies`.

# forestploter 0.1.8

* Added options for arrows for alignment and other controls.
* Added `x_trans` options for different scales of x-axis.
* Added `get_wh` and `get_scale` for saving plots.
* `xlog` has been deprecated, should be define in `x_trans`.

# forestploter 0.1.7

* Fixed minor issue in inserting text.
* Able to suppress legend

# forestploter 0.1.6

* Able to define the rounding digits for ticks.
* Able to add title to the plot.

# forestploter 0.1.5

* Fixed issues with checks for zeros if `xlog=TRUE`.
* Different reference line, x-axis label, xlog, vertical line, xlim, x-axis ticks and arrow label is possible for different CI columns.
* Able to add x-axis label.

# forestploter 0.1.4

* Fixed issues with xlim calculation.
* Fixed bug in theme setting axis cex not used.
* Added some unit tests.

# forestploter 0.1.3

* Added CI line width option.
* Added CI T end option.
* Fixed bug in `xlim`.

# forestploter 0.1.2

* Added `xlog` options for exponential estimates, eg HR, OR.
* Auto calculate x-ticks and xlim for multiple column.
* Minor updates and changes.

# forestploter 0.1.1

* Added summary diamond shape.
* Able to change CI line type.
* Able to insert text vector to multiple column.
* Able to select row header for plot editing.
* Print plot with auto fit.
* Fixed row calculation in plot editing.
* Fixed some typos.

# forestploter 0.0.1

* Initial release.

