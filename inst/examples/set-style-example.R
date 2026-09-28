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
            ref_line = 1,
            style = forest_style(base_size = 10, ref_line = gpar(col = "red")))

# Settings left out are kept, so the reference line stays red
p <- set_style(p, ci = gpar(col = "#4575b4"), title_just = "center")
plot(set_labs(p, title = "Subgroup analysis"))

# `NULL` goes back to the default, a style replaces the whole style
plot(set_style(p, ref_line = NULL))
plot(set_style(p, forest_style(base_size = 8)))

# Let the CI column take the width of the page it is drawn on
plot(set_style(p, fit = "width"))
