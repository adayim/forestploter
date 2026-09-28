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

# Limits and tick marks, with a vertical line at 2
p <- set_xaxis(p, xlim = c(0, 4), ticks_at = c(0.5, 1, 2, 3), vline = 2)
plot(p)

# The axis can be set in several calls, `NULL` goes back to the default
p <- set_xaxis(p, ticks_digits = 1L)
p <- set_xaxis(p, vline = NULL)

# A log scale, where the reference line of `forest()` defaults to 1
plot(set_xaxis(p, x_trans = "log", xlim = c(0.25, 4), ticks_at = c(0.5, 1, 2, 4)))
