# Get width and height of the forestplot

`get_wh` can be used to find the correct width and height of the
forestplot for saving, as the width and height are difficult to estimate
otherwise.

## Usage

``` r
get_wh(plot, unit = c("in", "cm", "mm"))
```

## Arguments

- plot:

  A forest plot object.

- unit:

  Unit for the returned width and height. One of `"in"`, `"cm"`, or
  `"mm"`.

## Value

A named vector of width and height

## Details

This is the natural size of the plot, where every column and row fits
its content. By default a plot drawn in a larger space keeps this size
and is centred. With `fit` of
[`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)
the CI columns, and the rows if asked for, take the space instead.

## Examples

``` r
if (FALSE) { # \dontrun{
 dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
 dt <- dt[1:6,1:6]

 dt$` ` <- paste(rep(" ", 20), collapse = " ")

 p <- forest(dt[,c(1:3, 7)],
             est = dt$est,
             lower = dt$low,
             upper = dt$hi,
             ci_column = 4)

# get_wh example
p_wh <- get_wh(p)
pdf('test.pdf',width = p_wh[1], height = p_wh[2])
plot(p)
dev.off()
} # }
```
