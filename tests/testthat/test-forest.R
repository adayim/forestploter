
#### Prep data
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


test_that("Simple forestplot", {

  p <- forest(dt[,c(1:3, 20:21)],
              est = dt$est,
              lower = dt$low,
              upper = dt$hi,
              sizes = dt$se,
              ci_column = 4,
              ref_line = 1,
              arrow_lab = c("Placebo Better", "Treatment Better"))

  vdiffr::expect_doppelganger("Simple forest plot", p)
})


test_that("Apply theme", {

  tm <- forest_theme(base_size = 10,
                     refline_col = "red",
                     ci_lty = 1,
                     ci_lwd = 1,
                     ci_Theight = 0.2,
                     footnote_col = "blue")

  p <- forest(dt[,c(1:3, 20:21)],
              est = dt$est,
              lower = dt$low,
              upper = dt$hi,
              sizes = dt$se,
              ci_column = 4,
              ref_line = 1,
              arrow_lab = c("Placebo Better", "Treatment Better"),
              xlim = c(0, 4),
              ticks_at = c(0.5, 1, 2, 3),
              footnote = "This is only a demo",
              theme = tm)

  vdiffr::expect_doppelganger("Simple forest plot with theme", p)

  # Edit plot
  # Edit text in row 3
  g <- edit_plot(p, row = 3, gp = gpar(col = "red", fontface = "italic"))

  # Bold grouping text
  g <- edit_plot(g,
                 row = c(2, 5, 10, 13, 17, 20),
                 gp = gpar(fontface = "bold"))

  # Insert text at top
  g <- insert_text(g,
                   text = "Treatment group",
                   col = 2:3,
                   part = "header",
                   gp = gpar(fontface = "bold"))

  # Add underline at the bottom of the header
  g <- add_underline(g, part = "header")

  # Edit background of row 5
  g <- edit_plot(g, row = 5, which = "background",
                 gp = gpar(fill = "darkolivegreen1"))

  # Insert text
  g <- insert_text(g,
                   text = "This is a long text. Age and gender summarised above.\nBMI is next",
                   row = 10,
                   just = "left",
                   gp = gpar(cex = 0.6, col = "green", fontface = "italic"))

  g <- add_text(g,
                text = "This is a long text. Age and gender summarised above.\nBMI is next",
                row = 12,
                col = 2:4,
                just = "left",
                gp = gpar(cex = 1, col = "red", fontface = "italic"))

  vdiffr::expect_doppelganger("Edit plot with theme", p)
})


test_that("Multiple column", {

  tm <- forest_theme(base_size = 10,
                     refline_col = "green",
                     ci_lty = c(1, 3),
                     ci_lwd = 1.5,
                     ci_Theight = 0.2,
                     footnote_col = "blue",
                     legend_name = "GP",
                     legend_value = c("Trt 1", "Trt 2"))

  p <- forest(dt[,c(1:2, 20, 3, 22)],
              est = list(dt$est_gp1,
                         dt$est_gp2,
                         dt$est_gp3,
                         dt$est_gp4),
              lower = list(dt$low_gp1,
                           dt$low_gp2,
                           dt$low_gp3,
                           dt$low_gp4),
              upper = list(dt$hi_gp1,
                           dt$hi_gp2,
                           dt$hi_gp3,
                           dt$hi_gp4),
              ci_column = c(3, 5),
              ref_line = 1,
              arrow_lab = c("Placebo Better", "Treatment Better"),
              nudge_y = 0.2,
              xlim = c(0, 4),
              theme = tm)

  vdiffr::expect_doppelganger("Multiple columns", p)
})


test_that("Multiple column and Multi parameters", {

  tm <- forest_theme(base_size = 10,
                     refline_col = "green",
                     ci_lty = c(1, 3),
                     ci_lwd = 1.5,
                     ci_Theight = 0.2,
                     footnote_col = "blue",
                     legend_name = "GP",
                     legend_value = c("Trt 1", "Trt 2"))

  p <- forest(dt[,c(1:2, 20, 3, 22)],
              est = list(dt$est_gp1,
                         dt$est_gp2,
                         dt$est_gp3,
                         dt$est_gp4),
              lower = list(dt$low_gp1,
                           dt$low_gp2,
                           dt$low_gp3,
                           dt$low_gp4),
              upper = list(dt$hi_gp1,
                           dt$hi_gp2,
                           dt$hi_gp3,
                           dt$hi_gp4),
              ci_column = c(3, 5),
              ref_line = c(1, 0),
              vert_line = list(c(0.3, 1.4), c(0.6, 2)),
              xlog = c(T, F),
              arrow_lab = list(c("L1", "R1"), c("L2", "R2")),
              xlim = list(c(0, 3), c(-1, 3)),
              ticks_at = list(c(0.1, 0.5, 1, 2.5), c(-1, 0, 2)),
              xlab = c("OR", "Beta"),
              nudge_y = 0.2,
              theme = tm)

  vdiffr::expect_doppelganger("Multiple columns and multi parameters", p)
})
