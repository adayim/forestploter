# Draw plot

Print or draw forestplot.

## Usage

``` r
# S3 method for class 'forestplot'
print(x, autofit = FALSE, ...)

# S3 method for class 'forestplot'
plot(x, autofit = FALSE, ...)
```

## Arguments

- x:

  forestplot to display

- autofit:

  If true, the page is shared equally between the columns and between
  the rows of the plot. This will be deprecated, use `fit` of
  [`forest_style`](https://adayim.github.io/forestploter/reference/forest_style.md)
  instead, which also works with `ggplot2::ggsave` and `patchwork`.

- ...:

  other arguments not used by this method

## Value

Invisibly returns the original forestplot.
