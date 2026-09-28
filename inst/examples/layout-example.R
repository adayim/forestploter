library(grid)
# Read provided sample example data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:8, ]

# NA to blank or NA will be transformed to character
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)

# Add blank columns for the forest plot to display CI.
# Adjust the column width with space.
dt$` ` <- paste(rep(" ", 20), collapse = " ")
dt$`  ` <- paste(rep(" ", 20), collapse = " ")

# A style that can be reused for other plots
st <- forest_style(base_size = 10,
                   ref_line = gpar(col = "red"),
                   vline = gpar(col = "grey60"),
                   footnote = gpar(col = "#636363", fontface = "italic"),
                   arrow_type = "closed",
                   title_just = "center")

# Add the axis and the text to the plot
p <- forest(dt[, c(1:3, 19)],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            sizes = dt$est,
            ci_column = 4,
            ref_line = 1,
            style = st) |>
  set_xaxis(x_trans = "log",
            xlim = c(0.25, 4),
            ticks_at = c(0.5, 1, 2, 4),
            vline = c(0.5, 2)) |>
  set_labs(title = "Subgroup analysis",
           xlab = "Hazard ratio",
           arrow = c("Placebo Better", "Treatment Better"),
           footnote = "This is the demo data.") |>
  scale_sizes(method = "range", range = c(0.3, 0.8))

plot(p)

# Change the style, let the CI column take the free width of the page and
# remove the footnote
p <- p |>
  set_style(base_size = 12, title = gpar(col = "blue"), fit = "width") |>
  set_labs(footnote = NULL)

plot(p)

# Grouped CIs in two columns, with a legend
p <- forest(dt[, c(1, 19, 20)],
            est = list(dt$est_gp1, dt$est_gp2, dt$est_gp3, dt$est_gp4),
            lower = list(dt$low_gp1, dt$low_gp2, dt$low_gp3, dt$low_gp4),
            upper = list(dt$hi_gp1, dt$hi_gp2, dt$hi_gp3, dt$hi_gp4),
            ci_column = c(2, 3),
            ref_line = 1,
            nudge_y = 0.2,
            style = forest_style(ci = gpar(col = c("#377eb8", "#4daf4a")),
                                 legend_position = "bottom")) |>
  set_xaxis(x_trans = "log", xlim = list(c(0.1, 5), NA)) |>
  set_labs(xlab = c("CVD outcome", "COPD outcome"),
           legend_title = "Group",
           legend_labels = c("Trt 1", "Trt 2"))

plot(p)
