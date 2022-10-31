
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo),
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Add blank column for the forest plot to display CI
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                           sprintf("%.2f (%.2f to %.2f)",
                                   dt$est, dt$low, dt$hi))
dt$`   ` <- paste(rep(" ", 10), collapse = " ")



test_that("check_errors works", {
  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      ci_column = "a"),
               "ci_column must be numeric atomic vector")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low[-1],
                      upper = dt$hi,
                      ci_column = 3),
               "Estimate, lower and upper should have the same length.")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      is_summary = rep(F, nrow(dt)-1),
                      ci_column = 3),
               "is_summary should have same legnth as data rownumber")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      ref_line = c(1, 0),
                      ci_column = 3),
               "ref_line should be of length 1 or the same length as ci_column.")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      ref_line = 1,
                      x_trans = c("log", "none"),
                      ci_column = 3),
               "x_trans must be in \"none\", \"log\", \"log2\", \"log10\" and of length 1 or the same length as ci_column.")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      ref_line = 1,
                      x_trans = "log5",
                      ci_column = 3),
               "x_trans must be in \"none\", \"log\", \"log2\", \"log10\" and of length 1 or the same length as ci_column.")

  expect_error(forest(dt[,c(1:3, 20:21)],
                      est = dt$est,
                      lower = dt$low,
                      upper = dt$hi,
                      xlab = c("OR", "HR"),
                      ci_column = 3),
               "xlab must be of length 1 or the same length as ci_column.")


})
