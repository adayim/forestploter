library(grid)
# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:6, ]

# Add a blank column for the forest plot to display CI
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# The weight of each study, here the inverse of the width of the CI
weights <- 1/(dt$hi - dt$low)

p <- forest(dt[, c("Subgroup", " ")],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            sizes = weights,        # weights, not sizes
            ci_column = 2,
            ref_line = 1)

# The area of each point is proportional to its weight
plot(scale_sizes(p, method = "range", range = c(0.2, 0.8)))

# `NULL` turns the scaling off, the values of `sizes` are then used as they are
plot(scale_sizes(p, method = NULL))
