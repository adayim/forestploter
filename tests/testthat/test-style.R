
#### Prep data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:8, ]
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$` ` <- paste(rep(" ", 20), collapse = " ")

one_col <- function(...){
  forest(dt[, c(1:3, 19)],
         est = dt$est,
         lower = dt$low,
         upper = dt$hi,
         ci_column = 4,
         ref_line = 1,
         ...)
}

# Theme of a plot as it is drawn
plot_theme <- function(p){
  resolve_theme(attr(p, "forest_recipe"))
}

# Convert a theme to a style and back
round_trip <- function(tm){
  old <- theme_to_style(tm)
  style_to_theme(old$style, old$legend)
}


test_that("Default style is the default theme", {

  expect_identical(style_to_theme(forest_style()), forest_theme())
  expect_identical(style_to_theme(NULL), forest_theme())
  expect_identical(plot_theme(one_col()), forest_theme())
})


test_that("Themes survive the conversion to a style", {

  themes <- list(
    forest_theme(),
    forest_theme(base_size = 10, base_family = "serif"),
    forest_theme(base_size = 10,
                 refline_gp = gpar(col = "red"),
                 ci_lty = 1,
                 ci_lwd = 1,
                 ci_Theight = 0.2,
                 footnote_gp = gpar(col = "blue")),
    forest_theme(base_size = 10,
                 refline_gp = gpar(col = "green"),
                 ci_lty = c(1, 3),
                 ci_lwd = 1.5,
                 ci_Theight = 0.2,
                 footnote_gp = gpar(col = "blue"),
                 legend_name = "GP",
                 legend_value = c("Trt 1", "Trt 2")),
    forest_theme(base_size = 10,
                 ci_pch = 16,
                 ci_col = "#762a83",
                 ci_fill = "black",
                 ci_alpha = 0.8,
                 ci_Theight = 0.2,
                 vertline_gp = gpar(lwd = 1),
                 summary_fill = "#4575b4",
                 summary_col = "#4575b4",
                 footnote_gp = gpar(cex = 0.6, fontface = "italic", col = "blue"),
                 title_just = "center",
                 title_gp = gpar(col = "red")),
    forest_theme(arrow_gp = gpar(cex = .5),
                 arrow_label_just = "end",
                 xaxis_gp = gpar(cex = .5),
                 arrow_length = 0.1,
                 arrow_type = "closed",
                 xlab_adjust = "center",
                 footnote_parse = FALSE),
    forest_theme(base_size = 10,
                 refline_gp = gpar(lty = "solid"),
                 ci_pch = c(15, 18, 16, 17, 19),
                 ci_col = c("#808080", "#00FF00", "royalblue3", "maroon3", "red"),
                 ci_lwd = 2,
                 legend_name = "Model:   ", legend_position = "bottom",
                 legend_value = c("Cox  ", "Normal  ", "Clayton  ",  "Frank", "Gumbel"),
                 legend_ncol = 2,
                 legend_byrow = FALSE,
                 vertline_lty = c("dashed", "dotdash"),
                 vertline_col = c("#d6604d", "#A52A2A")),
    forest_theme(legend_value = c("Gp1", "Gp2", "Gp3")),
    forest_theme(legend_value = c("Gp1", "Gp2"), ci_fill = "black"),
    forest_theme(core = list(fg_params = list(hjust = 1, x = 0.9),
                             bg_params = list(fill = c("#edf8e9", "#c7e9c0"))),
                 colhead = list(fg_params = list(hjust = 0.5, x = 0.5)),
                 summary_col = "black")
  )

  for(tm in themes)
    expect_identical(round_trip(tm), tm)

  tm <- suppressWarnings(forest_theme(ci_fill = "#e41a1c", ci_pch = 1))
  expect_identical(suppressWarnings(round_trip(tm)), tm)
})


test_that("forest_style has its defaults and checks what is given", {

  st <- forest_style()
  expect_s3_class(st, "forest_style")
  expect_identical(st$base_size, 12)
  expect_identical(st$title_just, "left")
  expect_identical(st$fit, "none")
  expect_identical(st$title, gpar())
  expect_null(st$parse)
  expect_null(st$table)

  # NULL gives the default
  expect_identical(forest_style(base_size = NULL, title = NULL, fit = NULL), st)
  expect_identical(forest_style(core = list(padding = unit(c(2, 2), "mm")))$table,
                   list(core = list(padding = unit(c(2, 2), "mm"))))
  expect_identical(forest_style(title_just = "cent")$title_just, "center")
  expect_identical(forest_style(fit = "both")$fit, "both")

  expect_error(forest_style(ref_line = list(col = "red")),
               "ref_line must be a gpar\\(\\) object")
  expect_error(forest_style(title = NA), "title must be a gpar\\(\\) object")
  expect_error(forest_style(base_size = "a"), "`base_size` must be a single number")
  expect_error(forest_style(parse = "yes"), "`parse` must be NULL, TRUE or FALSE")
  expect_error(forest_style(ci = gpar(alpha = c(0.2, 0.3))), "must be of length 1")
  expect_error(forest_style(ci_t_height = c(0.2, 0.3)), "must be of length 1")
  expect_error(forest_style(legend_position = "left"),
               "`legend_position` must be one of \"right\", \"top\", \"bottom\", \"none\"")
  expect_error(forest_style(fit = "height"),
               "`fit` must be one of \"none\", \"width\", \"both\"")
  expect_error(forest_style(arrow_length = "long"), "must be a single number or unit")
  expect_error(forest_style(legend_ncol = "a"), "must be a single number")
  expect_error(forest_style(legend_byrow = NA), "must be TRUE or FALSE")
  expect_error(set_style(one_col(), list(a = 1)), "style must be created with")
  expect_error(set_style(one_col(), forest_style(), gpar()), "must be named")

  # Only table settings go through `...`
  expect_error(forest_style(titel = gpar(col = "red")),
               "Unknown arguments in `forest_style\\(\\)`: `titel`")
  expect_error(forest_style(ci_col = "red", refline_gp = gpar()),
               "Arguments of `forest_theme\\(\\)` are not used by `forest_style\\(\\)`: `ci_col`, `refline_gp`")
  expect_error(forest_style(core = "left"), "`core` must be a list")
  expect_error(set_style(one_col(), titel = gpar()), "Unknown arguments")
})


test_that("set_style keeps what is left out and NULL goes back to the default", {

  p <- one_col(style = forest_style(title_just = "center",
                                    title = gpar(col = "red"),
                                    ci_t_height = 0.2,
                                    parse = FALSE,
                                    fit = "width",
                                    core = list(fg_params = list(hjust = 1, x = 0.9))))

  # Nothing given keeps everything
  g <- set_style(p)
  expect_identical(attr(g, "forest_recipe")$style, attr(p, "forest_recipe")$style)
  expect_same_svg(g, p)

  # NULL goes back to the default, the rest is kept
  g <- set_style(p, title_just = NULL, title = NULL)
  tm <- plot_theme(g)
  expect_identical(tm$title$just, "left")
  expect_identical(tm$title$gp, forest_theme()$title$gp)
  expect_identical(tm$ci$t_height, 0.2)
  expect_false(tm$footnote$parse)
  expect_identical(attr(g, "forest_recipe")$style$fit, "width")

  g <- set_style(p, ci_t_height = NULL, parse = NULL, core = NULL, fit = NULL)
  tm <- plot_theme(g)
  expect_null(tm$ci$t_height)
  expect_true(tm$footnote$parse)
  expect_identical(tm$tab_theme, forest_theme()$tab_theme)
  expect_null(attr(g, "forest_recipe")$style$fit)

  # Choices are checked and completed as in `forest_style()`
  g <- set_style(p, fit = "bo", title_just = "ri")
  expect_identical(attr(g, "forest_recipe")$style[c("fit", "title_just")],
                   list(fit = "both", title_just = "right"))
  expect_error(set_style(p, fit = "height"), "`fit` must be one of")

  # All of them back to the default, also when given to `forest()` directly
  expect_same_svg(set_style(p, forest_style()), one_col())
  expect_same_svg(one_col(style = forest_style(title = NULL, base_size = NULL)), one_col())
})


test_that("A style prints the settings that are not the default", {

  expect_output(print(forest_style()), "the default style")

  out <- capture.output(print(forest_style(base_size = 10,
                                           title = gpar(col = "red"),
                                           fit = "width",
                                           core = list(padding = unit(c(2, 2), "mm")))))
  expect_identical(out[1], "<forest_style>")
  expect_true(any(grepl("base_size: 10", out, fixed = TRUE)))
  expect_true(any(grepl("title: gpar(col = \"red\")", out, fixed = TRUE)))
  expect_true(any(grepl("fit: \"width\"", out, fixed = TRUE)))
  expect_true(any(grepl("table: a list of core", out, fixed = TRUE)))
  expect_length(out, 5)
})


test_that("set_style updates and replaces the style", {

  p <- one_col(style = forest_style(base_size = 10, xaxis = gpar(col = "blue")))

  # Settings are merged into the current style
  g <- set_style(p, ref_line = gpar(col = "red"), title_just = "right")
  g <- set_style(g, ref_line = gpar(lty = "solid"))
  tm <- plot_theme(g)
  expect_identical(tm$refline$col, "red")
  expect_identical(tm$refline$lty, "solid")
  expect_identical(tm$base_size, 10)
  expect_identical(tm$xaxis$col, "blue")
  expect_identical(tm$title$just, "right")

  # A style replaces the current one
  g <- set_style(g, forest_style(base_size = 8))
  tm <- plot_theme(g)
  expect_identical(tm$base_size, 8)
  expect_identical(tm$refline$col, "grey20")
  expect_null(tm$xaxis$col)
  expect_identical(tm$title$just, "left")

  # Placement settings reach the theme
  g <- set_style(p, arrow_type = "closed", arrow_length = unit(2, "mm"),
                 arrow_label_just = "end", xlab_adjust = "center",
                 legend_position = "top", legend_ncol = 2, legend_byrow = FALSE)
  tm <- plot_theme(g)
  expect_identical(tm$arrow$type, "closed")
  expect_identical(tm$arrow$length, unit(2, "mm"))
  expect_identical(tm$arrow$label_just, "end")
  expect_identical(tm$xlab$just, "center")
  expect_identical(tm$legend[c("position", "ncol", "byrow")],
                   list(position = "top", ncol = 2, byrow = FALSE))
})


test_that("Themes of forest_theme() work with set_style", {

  tm <- forest_theme(base_size = 10,
                     footnote_gp = gpar(col = "blue"),
                     title_just = "center",
                     arrow_type = "closed",
                     legend_name = "Trial")

  p <- one_col(style = tm) |>
    set_labs(title = "Title", footnote = "Note", arrow = c("L", "R"))

  # Same plot through the conversion
  expect_same_svg(set_style(p), p)

  # The base size reaches every part, the rest of the theme is kept
  g <- set_style(p, base_size = 8)
  tm2 <- plot_theme(g)
  expect_identical(tm2$footnote$gp$fontsize, 8)
  expect_identical(tm2$xaxis$fontsize, 8)
  expect_identical(tm2$tab_theme$core$fg_params$fontsize, 8)
  expect_identical(tm2$footnote$gp$col, "blue")
  expect_identical(tm2$title$just, "center")
  expect_identical(tm2$arrow$type, "closed")
  expect_identical(tm2$legend$name, "Trial")

  # The legend title of `set_labs()` wins over the theme
  g <- set_labs(p, legend_title = "Model")
  expect_identical(plot_theme(g)$legend$name, "Model")
  expect_identical(plot_theme(set_style(g, base_size = 8))$legend$name, "Model")

  # A theme given to set_style replaces the style
  g <- one_col(style = forest_style(base_size = 8)) |>
    set_labs(title = "Title", footnote = "Note", arrow = c("L", "R")) |>
    set_style(tm)
  expect_same_svg(g, p)
})


test_that("Table style reaches text and background", {

  p <- one_col(style = forest_style(body = gpar(fill = c("red", "white"),
                                                fontface = "bold",
                                                col = "blue"),
                                    header = gpar(fill = "grey", fontsize = 14)))
  tab <- plot_theme(p)$tab_theme

  expect_identical(tab$core$bg_params$fill, c("red", "white"))
  expect_identical(tab$core$bg_params$col, c("red", "white"))
  expect_equal(tab$core$fg_params$fontface, 2)
  expect_identical(tab$core$fg_params$col, "blue")
  expect_identical(tab$colhead$bg_params$fill, "grey")
  expect_identical(tab$colhead$fg_params$fontsize, 14)
  expect_no_error(draw_null(p))

  # Same as the table settings of forest_theme()
  expect_same_svg(p,
                  one_col(style = forest_theme(
                    core = list(fg_params = list(fontface = 2L, col = "blue"),
                                bg_params = list(fill = c("red", "white"))),
                    colhead = list(fg_params = list(fontsize = 14),
                                   bg_params = list(fill = "grey", col = "grey")))))

  # Settings passed on to the table
  p <- one_col(style = forest_style(core = list(fg_params = list(hjust = 1, x = 0.9))))
  expect_identical(plot_theme(p)$tab_theme$core$fg_params$hjust, 1)
})


test_that("body and header are applied on top of core and colhead", {

  # Settings of both are kept, `body` wins where they meet
  tab <- plot_theme(one_col(style = forest_style(
    body = gpar(col = "blue", fill = "red"),
    core = list(fg_params = list(col = "green", hjust = 1, x = 0.9),
                bg_params = list(fill = "yellow")))))$tab_theme

  expect_identical(tab$core$fg_params$col, "blue")
  expect_identical(tab$core$fg_params$hjust, 1)
  expect_identical(tab$core$bg_params$fill, "red")

  # The cell border follows the fill, unless `core` gives it a colour
  expect_identical(tab$core$bg_params$col, "red")

  tab <- plot_theme(one_col(style = forest_style(
    body = gpar(fill = "red"),
    core = list(bg_params = list(col = "black")),
    colhead = list(bg_params = list(col = "black")),
    header = gpar(fill = "grey"))))$tab_theme

  expect_identical(tab$core$bg_params$fill, "red")
  expect_identical(tab$core$bg_params$col, "black")
  expect_identical(tab$colhead$bg_params$fill, "grey")
  expect_identical(tab$colhead$bg_params$col, "black")

  # The order of the arguments does not matter
  expect_identical(
    style_to_theme(forest_style(body = gpar(col = "blue"),
                                core = list(fg_params = list(col = "red")))),
    style_to_theme(forest_style(core = list(fg_params = list(col = "red")),
                                body = gpar(col = "blue"))))
})


test_that("parse applies to the labels outside the table", {

  label_of <- function(p, name){
    p$grobs[[which(p$layout$name == name)]]$label
  }

  labs <- function(p){
    set_labs(p, title = "alpha^2", footnote = "beta^2", xlab = "gamma^2",
             arrow = c("delta^2", "Plain text"))
  }

  # Default: only the footnote
  p <- labs(one_col())
  expect_type(label_of(p, "plot.title"), "character")
  expect_type(label_of(p, "footnote"), "expression")

  p <- labs(one_col(style = forest_style(parse = NULL)))
  expect_same_svg(p, labs(one_col()))

  # All labels
  p <- labs(one_col(style = forest_style(parse = TRUE)))
  expect_type(label_of(p, "plot.title"), "expression")
  expect_type(label_of(p, "footnote"), "expression")
  arrow <- p$grobs[[which(p$layout$name == "arrow-4")]]
  expect_type(arrow$children$arrow.text.left$label, "expression")
  expect_identical(as.character(arrow$children$arrow.text.right$label), "Plain text")
  xaxis <- p$grobs[[which(p$layout$name == "xaxis-4")]]
  expect_type(xaxis$children$xlab$label, "expression")
  expect_no_error(draw_null(p))

  # None
  p <- labs(one_col(style = forest_style(parse = FALSE)))
  expect_type(label_of(p, "footnote"), "character")

  # Expressions are drawn as they are
  p <- set_labs(one_col(), title = expression(alpha^2))
  expect_type(label_of(p, "plot.title"), "expression")

  expect_identical(parse_label(c("a^2", "not valid (", "")),
                   expression(a^2, "not valid (", ""))
})
