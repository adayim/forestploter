
# `set_xaxis()`, `set_labs()`, `set_style()` and `scale_sizes()` must draw
# exactly what the old arguments of `forest()` and `forest_theme()` draw, the
# fixtures below are those of the visual snapshots.

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

base_plot <- function(...){
  forest(dt[,c(1:3, 20:21)],
         est = dt$est,
         lower = dt$low,
         upper = dt$hi,
         sizes = dt$se,
         ci_column = 4,
         ...)
}

grouped_plot <- function(...){
  forest(dt[,c(1:2, 20, 3, 22)],
         est = list(dt$est_gp1, dt$est_gp2, dt$est_gp3, dt$est_gp4),
         lower = list(dt$low_gp1, dt$low_gp2, dt$low_gp3, dt$low_gp4),
         upper = list(dt$hi_gp1, dt$hi_gp2, dt$hi_gp3, dt$hi_gp4),
         ci_column = c(3, 5),
         ...)
}


test_that("Simple forest plot", {

  p <- base_plot(ticks_digits = 1,
                 ref_line = 1,
                 arrow_lab = c("Placebo Better", "Treatment Better"))

  g <- base_plot(ref_line = 1) |>
    set_xaxis(ticks_digits = 1) |>
    set_labs(arrow = c("Placebo Better", "Treatment Better"))

  expect_same_svg(g, p)

  # The order of the pipe functions does not matter
  g <- base_plot(ref_line = 1) |>
    set_labs(arrow = c("Placebo Better", "Treatment Better")) |>
    set_xaxis(ticks_digits = 1)

  expect_same_svg(g, p)
})


test_that("Theme, style, axis and labels", {

  tm <- forest_theme(base_size = 10,
                     refline_gp = gpar(col = "red"),
                     ci_lty = 1,
                     ci_lwd = 1,
                     ci_Theight = 0.2,
                     footnote_gp = gpar(col = "blue"))

  p <- base_plot(ref_line = 1,
                 xlim = c(0, 4),
                 ticks_at = c(0.5, 1, 2, 3),
                 ticks_digits = 1L,
                 arrow_lab = c("Placebo Better", "Treatment Better"),
                 footnote = "This is only a demo",
                 style = tm)

  layout <- function(plot){
    plot |>
      set_xaxis(xlim = c(0, 4), ticks_at = c(0.5, 1, 2, 3), ticks_digits = 1L) |>
      set_labs(arrow = c("Placebo Better", "Treatment Better"),
               footnote = "This is only a demo")
  }

  st <- forest_style(base_size = 10,
                     ref_line = gpar(col = "red"),
                     ci = gpar(lty = 1, lwd = 1),
                     ci_t_height = 0.2,
                     footnote = gpar(col = "blue"))

  # Theme of forest_theme() or style given to forest()
  expect_same_svg(layout(base_plot(ref_line = 1, style = tm)), p)
  expect_same_svg(layout(base_plot(ref_line = 1, style = st)), p)

  # Style set afterwards, as a style, as settings or as a theme
  expect_same_svg(set_style(layout(base_plot(ref_line = 1)), st), p)
  expect_same_svg(set_style(layout(base_plot(ref_line = 1)),
                            base_size = 10,
                            ref_line = gpar(col = "red"),
                            ci = gpar(lty = 1, lwd = 1),
                            ci_t_height = 0.2,
                            footnote = gpar(col = "blue")), p)
  expect_same_svg(set_style(layout(base_plot(ref_line = 1)), tm), p)

  # A theme converted to a style draws the same
  expect_same_svg(set_style(layout(base_plot(ref_line = 1, style = tm))), p)
})


test_that("Settings in several calls and edits afterwards", {

  tm <- forest_theme(base_size = 10,
                     refline_gp = gpar(col = "red"),
                     footnote_gp = gpar(col = "blue"))

  p <- base_plot(ref_line = 1,
                 xlim = c(0, 4),
                 arrow_lab = c("Placebo Better", "Treatment Better"),
                 title = "Title",
                 footnote = "This is only a demo",
                 style = tm)

  edits <- function(g){
    g <- edit_plot(g, row = 3, gp = gpar(col = "red", fontface = "italic"))
    g <- edit_plot(g, row = 3, col = 4, which = "ci", gp = gpar(col = "red"))
    g <- insert_text(g, text = "Treatment group", col = 2:3,
                     part = "header", gp = gpar(fontface = "bold"))
    g <- add_border(g, part = "header", row = 2, where = "bottom")
    g <- insert_text(g, text = c("Demographic", "Baseline"),
                     row = c(2, 17), just = "left")
    add_text(g, text = "58", col = 2, row = 10, just = "left")
  }

  g <- base_plot(ref_line = 1) |>
    set_xaxis(xlim = c(0, 3), ticks_at = c(1, 2)) |>
    set_labs(title = "Title", footnote = "Draft") |>
    set_style(tm) |>
    set_xaxis(xlim = c(0, 4), ticks_at = NULL) |>
    set_labs(arrow = c("Placebo Better", "Treatment Better"),
             footnote = "This is only a demo")

  expect_same_svg(g, p)
  expect_same_svg(edits(g), edits(p))
})


test_that("Multiple columns and legend", {

  tm <- forest_theme(base_size = 10,
                     refline_gp = gpar(col = "green"),
                     ci_lty = c(1, 3),
                     ci_lwd = 1.5,
                     ci_Theight = 0.2,
                     footnote_gp = gpar(col = "blue"),
                     legend_name = "GP",
                     legend_value = c("Trt 1", "Trt 2"))

  p <- grouped_plot(ref_line = 1,
                    arrow_lab = c("Placebo Better", "Treatment Better"),
                    nudge_y = 0.2,
                    xlim = c(0, 4),
                    ticks_digits = 1,
                    style = tm)

  st <- forest_style(base_size = 10,
                     ref_line = gpar(col = "green"),
                     ci = gpar(lty = c(1, 3), lwd = 1.5),
                     ci_t_height = 0.2,
                     footnote = gpar(col = "blue"))

  g <- grouped_plot(ref_line = 1, nudge_y = 0.2, style = st) |>
    set_xaxis(xlim = c(0, 4), ticks_digits = 1) |>
    set_labs(arrow = c("Placebo Better", "Treatment Better"),
             legend_title = "GP",
             legend_labels = c("Trt 1", "Trt 2"))

  expect_same_svg(g, p)

  # Legend labels given to a theme without them
  tm2 <- forest_theme(base_size = 10, ci_lwd = 1.5, legend_name = "GP")
  tm3 <- forest_theme(base_size = 10, ci_lwd = 1.5, legend_name = "GP",
                      legend_value = c("Trt 1", "Trt 2"))

  expect_same_svg(grouped_plot(ref_line = 1, nudge_y = 0.2, style = tm2) |>
                    set_labs(legend_labels = c("Trt 1", "Trt 2")),
                  grouped_plot(ref_line = 1, nudge_y = 0.2, style = tm3))

  # Different settings for different columns
  p <- grouped_plot(ref_line = c(1, 0),
                    vert_line = list(c(0.3, 1.4), c(0.6, 2)),
                    x_trans = c("log", "none"),
                    arrow_lab = list(c("L1", "R1"), c("L2", "R2")),
                    xlim = list(c(0, 3), c(-1, 3)),
                    ticks_at = list(c(0.1, 0.5, 1, 2.5), c(-1.0, 0, 1.5, 2.0)),
                    ticks_digits = list(1, 1L),
                    xlab = c("OR", "Beta"),
                    nudge_y = 0.2,
                    style = tm)

  g <- grouped_plot(ref_line = c(1, 0), nudge_y = 0.2, style = tm) |>
    set_xaxis(x_trans = c("log", "none"),
              xlim = list(c(0, 3), c(-1, 3)),
              ticks_at = list(c(0.1, 0.5, 1, 2.5), c(-1.0, 0, 1.5, 2.0)),
              ticks_digits = list(1, 1L),
              vline = list(c(0.3, 1.4), c(0.6, 2))) |>
    set_labs(arrow = list(c("L1", "R1"), c("L2", "R2")),
             xlab = c("OR", "Beta"))

  expect_same_svg(g, p)
})


test_that("Summary CI and title", {

  dt_tmp <- rbind(dt[-1, ], dt[1, ])
  dt_tmp[nrow(dt_tmp), 1] <- "Overall"

  tm <- forest_theme(base_size = 10,
                     ci_pch = 16,
                     ci_col = "#762a83",
                     ci_lty = 1,
                     ci_lwd = 1.5,
                     ci_Theight = 0.2,
                     refline_gp = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                     summary_fill = "#4575b4",
                     summary_col = "#4575b4",
                     footnote_gp = gpar(cex = 0.6, fontface = "italic", col = "blue"),
                     title_just = "center",
                     title_gp = gpar(col = "red"))

  summary_plot <- function(...){
    forest(dt_tmp[,c(1:3, 20:21)],
           est = dt_tmp$est,
           lower = dt_tmp$low,
           upper = dt_tmp$hi,
           sizes = dt_tmp$se,
           is_summary = c(rep(FALSE, nrow(dt_tmp)-1), TRUE),
           ci_column = 4,
           ref_line = 1,
           ...)
  }

  foot <- "This is the demo data. Please feel free to change\nanything you want."

  p <- summary_plot(xlim = c(0, 4),
                    ticks_at = c(0.5, 1, 2, 3),
                    ticks_digits = 1L,
                    arrow_lab = c("Placebo Better", "Treatment Better"),
                    title = "This is a title",
                    footnote = foot,
                    style = tm)

  st <- forest_style(base_size = 10,
                     ci_pch = 16,
                     ci = gpar(col = "#762a83", lty = 1, lwd = 1.5),
                     ci_t_height = 0.2,
                     ref_line = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                     summary = gpar(col = "#4575b4"),
                     footnote = gpar(cex = 0.6, fontface = "italic", col = "blue"),
                     title = gpar(col = "red"),
                     title_just = "center")

  layout <- function(plot){
    plot |>
      set_xaxis(xlim = c(0, 4), ticks_at = c(0.5, 1, 2, 3), ticks_digits = 1L) |>
      set_labs(title = "This is a title",
               footnote = foot,
               arrow = c("Placebo Better", "Treatment Better"))
  }

  expect_same_svg(layout(summary_plot(style = st)), p)
  expect_same_svg(layout(summary_plot(style = tm)), p)
})


test_that("Arrows", {

  dt <- dt[1:10, ]

  arrow_plot <- function(...){
    forest(dt[,c(1:3, 20:21)],
           est = dt$est,
           lower = dt$low,
           upper = dt$hi,
           ci_column = 4,
           ref_line = 1,
           ...)
  }

  for(just in c("end", "start")){
    tm <- forest_theme(arrow_gp = gpar(cex = .5),
                       arrow_label_just = just,
                       xaxis_gp = gpar(cex = .5),
                       arrow_length = 0.1,
                       arrow_type = "closed")

    p <- arrow_plot(arrow_lab = c("This Placebo Better", " text Bet"),
                    ticks_digits = 2L,
                    style = tm)

    st <- forest_style(arrow = gpar(cex = .5),
                       arrow_label_just = just,
                       xaxis = gpar(cex = .5),
                       arrow_length = 0.1,
                       arrow_type = "closed")

    g <- arrow_plot(style = st) |>
      set_xaxis(ticks_digits = 2L) |>
      set_labs(arrow = c("This Placebo Better", " text Bet"))

    expect_same_svg(g, p)
  }

  # Arrows can be removed
  expect_same_svg(set_labs(g, arrow = NULL),
                  set_xaxis(arrow_plot(style = st), ticks_digits = 2L))
})


test_that("x-scale trans", {

  dt <- dt[1:10, ]
  dt$hi <- dt$hi * 3

  dt$hi[9] <- 8
  dt$hi[8] <- 6
  dt$hi[7] <- 2
  dt$low[9] <- 0.25
  dt$low[8] <- 0.1

  trans_plot <- function(...){
    forest(dt[,c(1:3, 20:21)],
           est = dt$est,
           lower = dt$low,
           upper = dt$hi,
           ci_column = 4,
           ...)
  }

  p <- trans_plot(vert_line = 6,
                  ticks_at = c(0.1, 0.25, 1, 2, 6, 8),
                  x_trans = "log2")

  # The default reference line follows the scale
  g <- trans_plot() |>
    set_xaxis(x_trans = "log2", ticks_at = c(0.1, 0.25, 1, 2, 6, 8), vline = 6)

  expect_same_svg(g, p)

  dt$hi[9] <- 20
  dt$hi[8] <- 15
  dt$hi[7] <- 5
  dt$low[9] <- 0.5
  dt$low[8] <- 0.1

  p <- trans_plot(vert_line = 5,
                  ticks_at = c(0.1, 0.5, 1, 5, 20),
                  ticks_minor = c(0.1, 0.3, 1, 2.5, 5, 10),
                  x_trans = "log10",
                  xlim = c(0.09, 24),
                  ticks_digits = 1L)

  g <- trans_plot() |>
    set_xaxis(vline = 5,
              ticks_at = c(0.1, 0.5, 1, 5, 20),
              ticks_minor = c(0.1, 0.3, 1, 2.5, 5, 10),
              x_trans = "log10",
              xlim = c(0.09, 24),
              ticks_digits = 1L)

  expect_same_svg(g, p)

  # Values on a log scale must be positive
  expect_error(suppressMessages(set_xaxis(g, vline = -1)),
               "should be larger than 0")
})


test_that("Multiple groups", {

  dt <- dt[1:6, ]

  tm <- forest_theme(base_size = 10,
                     refline_gp = gpar(lty = "solid"),
                     ci_pch = c(15, 18, 16, 17, 19),
                     ci_col = c("#808080", "#00FF00", "royalblue3", "maroon3", "red"),
                     ci_lwd = 2,
                     footnote_gp = gpar(col = "blue"),
                     legend_name = "Model:   ", legend_position = "bottom",
                     legend_value = c("Cox  ", "Normal  ", "Clayton  ",  "Frank", "Gumbel"),
                     vertline_lty = c("dashed", "dotdash"),
                     vertline_col = c("#d6604d", "#A52A2A"))

  group_plot <- function(...){
    forest(dt[,c(1:2, 20)],
           est = list(dt$est, dt$est_gp1, dt$est_gp2, dt$est_gp3, dt$est_gp4),
           lower = list(dt$low, dt$low_gp1, dt$low_gp2, dt$low_gp3, dt$low_gp4),
           upper = list(dt$hi, dt$hi_gp1, dt$hi_gp2, dt$hi_gp3, dt$hi_gp4),
           ci_column = 3,
           ref_line = 1,
           nudge_y = 0.2,
           ...)
  }

  p <- group_plot(xlim = c(0, 4),
                  vert_line = c(0.5, 2),
                  arrow_lab = c("Placebo Better", "Treatment Better"),
                  style = tm)

  st <- forest_style(base_size = 10,
                     ref_line = gpar(lty = "solid"),
                     ci_pch = c(15, 18, 16, 17, 19),
                     ci = gpar(col = c("#808080", "#00FF00", "royalblue3", "maroon3", "red"),
                               lwd = 2),
                     footnote = gpar(col = "blue"),
                     vline = gpar(lty = c("dashed", "dotdash"),
                                     col = c("#d6604d", "#A52A2A")),
                     legend_position = "bottom")

  g <- group_plot(style = st) |>
    set_xaxis(xlim = c(0, 4), vline = c(0.5, 2)) |>
    set_labs(legend_title = "Model:   ",
             legend_labels = c("Cox  ", "Normal  ", "Clayton  ",  "Frank", "Gumbel"),
             arrow = c("Placebo Better", "Treatment Better"))

  expect_same_svg(g, p)
})


test_that("Custom CI functions keep their arguments", {

  box_func <- function(x){
    iqr <- IQR(x)
    q3 <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
    c("min" = q3[1] - 1.5*iqr, "q1" = q3[1], "med" = q3[2],
      "q3" = q3[3], "max" = q3[3] + 1.5*iqr)
  }
  val <- split(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose))
  val <- lapply(val, box_func)
  dat <- do.call(rbind, val)
  dat <- data.frame(Dose = row.names(dat), dat, row.names = NULL)
  dat$Box <- paste(rep(" ", 20), collapse = " ")

  box_plot <- function(...){
    forest(dat[,c(1, 7)],
           est = dat$med,
           lower = dat$min,
           upper = dat$max,
           fn_ci = make_boxplot,
           ci_column = 2,
           lowhinge = dat$q1,
           uphinge = dat$q3,
           hinge_height = 0.2,
           index_args = c("lowhinge", "uphinge"),
           gp_box = gpar(fill = "black", alpha = 0.4),
           ...)
  }

  p <- box_plot(style = forest_theme(ci_Theight = 0.2), title = "Box plot")
  g <- box_plot() |>
    set_style(ci_t_height = 0.2) |>
    set_labs(title = "Box plot")

  expect_same_svg(g, p)

  # Row values of `index_args` follow the scale set later
  expect_same_svg(set_xaxis(g, xlim = c(0, 40)),
                  box_plot(style = forest_theme(ci_Theight = 0.2),
                           title = "Box plot", xlim = c(0, 40)))
})


test_that("Grobs added in cells after the axis and labels", {

  dt <- read.csv(system.file("extdata", "metadata.csv",
                             package = "forestploter"))
  dt$cicol <- paste(rep(" ", 20), collapse = " ")
  dt_fig <- dt[,c(1:7, 17, 8:13)]
  colnames(dt_fig) <- c("Study or Subgroup",
                        "Events","Total","Events","Total",
                        "Weight",
                        "", "",
                        LETTERS[1:6])
  dt_fig$Weight <- sprintf("%0.1f%%", dt_fig$Weight)
  dt_fig$Weight[dt_fig$Weight == "NA%"] <- ""
  dt_fig[is.na(dt_fig)] <- ""

  tm <- forest_theme(core = list(bg_params=list(fill = c("white"))),
                     summary_col = "black",
                     arrow_label_just = "end",
                     arrow_type = "closed")

  meta_plot <- function(...){
    forest(dt_fig,
           est = dt$est,
           lower = dt$lb,
           upper = dt$ub,
           sizes = dt$weights,
           is_summary = c(rep(F, nrow(dt)-1), T),
           ci_column = 8,
           ref_line = 1,
           ...)
  }

  grobs <- function(g){
    g <- add_grob(g,
                  row = 1:c(nrow(dt_fig) - 1),
                  col = 9:14,
                  order = "background",
                  gb_fn = roundrectGrob,
                  r = unit(0.05, "snpc"),
                  gp = gpar(lty = "dotted",
                            col = "#bdbdbd"))
    g <- add_text(g, text = "Pm[2.5]",
                  part = "header",
                  col = 7:8,
                  gp = gpar(fontface = "bold"),
                  parse = TRUE)
    insert_text(g,
                text = c("S^2", "A", "12", "B", "C"),
                part = "header",
                col = 2:6,
                parse = TRUE)
  }

  p <- meta_plot(x_trans = "log",
                 xlim = c(0.05, 100),
                 ticks_at = c(0.1, 1, 10, 100),
                 arrow_lab = c("Favours caffeine","Favours decaf"),
                 style = tm) |>
    scale_sizes(method = "range")

  st <- forest_style(body = gpar(fill = "white"),
                     summary = gpar(col = "black"),
                     arrow_type = "closed",
                     arrow_label_just = "end")

  g <- meta_plot(style = st) |>
    set_xaxis(x_trans = "log", xlim = c(0.05, 100), ticks_at = c(0.1, 1, 10, 100)) |>
    set_labs(arrow = c("Favours caffeine","Favours decaf")) |>
    scale_sizes(method = "range")

  expect_same_svg(grobs(g), grobs(p))
})


test_that("Messages are given once in a pipe", {

  p <- set_xaxis(base_plot(), ticks_digits = 1L)
  expect_message(p <- set_xaxis(p, xlim = c(1.7, 5)),
                 "The confidence interval of row")

  # Building the same plot again does not repeat them
  expect_silent(g <- set_labs(p, title = "Title"))
  expect_silent(g <- set_style(g, base_size = 10))

  # A change that brings them back gives them again
  g <- set_xaxis(g, xlim = NULL)
  expect_message(set_xaxis(g, xlim = c(1.7, 5)), "The confidence interval of row")
})
