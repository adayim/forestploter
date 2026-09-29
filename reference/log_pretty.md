# Pretty ticks for log-transformed axes

Compute "log-pretty" tick values in the original (non-transformed) space
using decade-aware sub-multiples. Ranges spanning at least three orders
of magnitude collapse to one tick per decade (e.g. `1, 10, 100`);
narrower ranges include classic engineering sub-multiples (`1, 2, 5` or
`1, 2, 3, 5, 7`); ranges narrower than half a decade fall back to
[`pretty`](https://rdrr.io/r/base/pretty.html) on the original scale
because dense log ticks look clustered there.

## Usage

``` r
log_pretty(range_orig, base = 10)
```

## Arguments

- range_orig:

  Numeric length-2 range in the original (non-log) scale. All values
  must be strictly positive; otherwise the function falls back to
  [`pretty`](https://rdrr.io/r/base/pretty.html).

- base:

  Logarithm base. Use `exp(1)` for natural log, `2` for `log2`, or `10`
  for `log10`.

## Value

A numeric vector of tick values in the original scale.
