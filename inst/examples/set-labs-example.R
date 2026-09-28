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
