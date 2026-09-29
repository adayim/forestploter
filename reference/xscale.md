# Apply, invert, or format an x-axis scale

Helper used by the forest plot to switch between the user-facing axis
scale and the internal numeric scale, and to format tick labels.

## Usage

``` r
xscale(
  x,
  scale = c("none", "log", "log2", "log10", "scientific"),
  type = c("scale", "inv", "format"),
  format_digits = 1
)
```

## Arguments

- x:

  Numeric vector to be transformed or formatted.

- scale:

  Axis scale. One of `"none"`, `"log"`, `"log2"`, `"log10"`, or
  `"scientific"`.

- type:

  What to do with `x`: `"scale"` applies the transformation, `"inv"`
  inverts it back to the original space, and `"format"` returns
  formatted character labels.

- format_digits:

  Number of digits to keep when `type = "format"`. If an integer is
  supplied (e.g. `1L`) trailing zeros are dropped.
