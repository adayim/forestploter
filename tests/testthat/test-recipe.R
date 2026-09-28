
#### Prep data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:8, ]
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$` ` <- paste(rep(" ", 20), collapse = " ")
dt$`  ` <- paste(rep(" ", 20), collapse = " ")

one_col <- function(...){
  forest(dt[, c(1:3, 19)],
         est = dt$est,
         lower = dt$low,
         upper = dt$hi,
         ci_column = 4,
         ref_line = 1,
         ...)
}

two_col <- function(...){
  forest(dt[, c(1, 19, 20)],
         est = list(dt$est_gp1, dt$est_gp2, dt$est_gp3, dt$est_gp4),
         lower = list(dt$low_gp1, dt$low_gp2, dt$low_gp3, dt$low_gp4),
         upper = list(dt$hi_gp1, dt$hi_gp2, dt$hi_gp3, dt$hi_gp4),
         ci_column = c(2, 3),
         ref_line = 1,
         nudge_y = 0.4,
         ...)
}

# Messages given while evaluating `expr`
messages <- function(expr){
  out <- character()
  withCallingHandlers(expr, message = function(m){
    out <<- c(out, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  out
}


test_that("Plots carry their recipe and stay gtables", {

  p <- one_col()
  expect_s3_class(attr(p, "forest_recipe"), "forest_recipe")

  steps <- list(
    function(x) set_xaxis(x, xlim = c(0, 3), vline = 2),
    function(x) set_labs(x, title = "Title", xlab = "HR", footnote = "Note",
                         arrow = c("L", "R")),
    function(x) scale_sizes(x),
    function(x) set_style(x, base_size = 10),
    function(x) edit_plot(x, row = 1, gp = gpar(col = "red")),
    function(x) add_text(x, text = "A", row = 2, col = 1),
    function(x) insert_text(x, text = "B", row = 2),
    function(x) add_border(x, row = 1),
    function(x) add_grob(x, row = 1, col = 1, gb_fn = rectGrob)
  )

  for(step in steps){
    p <- step(p)
    expect_s3_class(p, "forestplot")
    expect_s3_class(p, "gtable")
    expect_true(is.grob(p))
  }

  expect_no_error(draw_null(p))

  # The axis moved to `set_xaxis()`, the labels to `set_labs()`
  expect_identical(names(formals(forest)),
                   c("data", "est", "lower", "upper", "sizes", "ref_line",
                     "ci_column", "is_summary", "nudge_y", "fn_ci",
                     "fn_summary", "index_args", "style", "..."))
})


test_that("The table is reused while data and table theme stay the same", {

  rm(list = ls(table_cache), envir = table_cache)

  p <- one_col()
  expect_equal(table_cache$n_built, 1)

  p <- set_labs(p, title = "Title")
  p <- set_xaxis(p, x_trans = "log", vline = 2)
  p <- set_style(p, ref_line = gpar(col = "red"))
  p <- scale_sizes(p)
  expect_equal(table_cache$n_built, 1)

  p <- set_style(p, body = gpar(fill = "red"))
  expect_equal(table_cache$n_built, 2)

  # A plot built from other data needs its own table
  g <- two_col()
  expect_equal(table_cache$n_built, 3)
})


test_that("Plots do not carry a copy of their grobs", {

  # Whether an object holds a grob anywhere, units included
  has_grob <- function(x){
    if(is.grob(x))
      return(TRUE)
    is.list(x) && any(vapply(unclass(x), has_grob, FUN.VALUE = logical(1)))
  }

  p <- two_col() |>
    set_xaxis(xlim = c(0, 6)) |>
    set_labs(title = "Title", legend_labels = c("A", "B")) |>
    set_style(base_size = 10)
  recipe <- attr(p, "forest_recipe")

  expect_true(has_grob(p$heights))
  expect_false(has_grob(recipe))

  # Saved plots can be built again
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  saveRDS(p, f)
  expect_same_svg(set_labs(readRDS(f), title = "New"),
                  set_labs(p, title = "New"))
})


test_that("Plots are not built again once edited or changed", {

  p <- one_col()

  g <- edit_plot(p, row = 1, gp = gpar(col = "red"))
  expect_error(set_labs(g, title = "Title"),
               "must be used before the plot is edited")
  expect_error(set_style(g, base_size = 10),
               "must be used before the plot is edited")
  expect_error(scale_sizes(g), "must be used before the plot is edited")
  expect_error(set_xaxis(g, xlim = c(0, 3)),
               "`set_xaxis\\(\\)` builds the plot again")

  g <- insert_text(p, text = "Text", row = 2)
  expect_error(set_labs(g, title = "Title"), "before the plot is edited")

  g <- p
  g$widths[2] <- unit(5, "cm")
  expect_error(set_labs(g, title = "Title"), "before the plot is edited")

  g <- gtable::gtable_add_rows(p, unit(1, "cm"))
  expect_error(set_style(g, base_size = 10), "before the plot is edited")

  g <- p
  g$grobs[[1]] <- nullGrob()
  expect_error(set_labs(g, title = "Title"), "before the plot is edited")

  # Edited plots can still be edited and drawn
  g <- add_border(edit_plot(p, row = 1, gp = gpar(col = "red")), row = 1)
  expect_no_error(draw_null(g))

  g <- p
  attr(g, "forest_recipe") <- NULL
  expect_error(set_labs(g, title = "Title"), "needs a plot created by `forest\\(\\)`")
  expect_s3_class(edit_plot(g, row = 1, gp = gpar(col = "red")), "forestplot")

  expect_error(set_labs(gtable::gtable(), title = "Title"),
               "plot must be a forestplot object")
})


test_that("Labels are kept, replaced and removed", {

  p <- two_col()

  # Labels left out are kept
  g <- set_labs(p, title = "Title", footnote = "Note")
  g <- set_labs(g, xlab = "HR")
  expect_true(all(c("plot.title", "footnote") %in% g$layout$name))
  recipe <- attr(g, "forest_recipe")
  expect_identical(recipe$labs$title, "Title")
  expect_identical(recipe$labs$xlab, list("HR", "HR"))

  # NULL removes
  g <- set_labs(g, title = NULL, footnote = NULL, xlab = NULL)
  expect_same_svg(g, p)
  expect_null(attr(g, "forest_recipe")$labs$title)

  # One label for each column, NA leaves a column without
  g <- set_labs(p, xlab = c("A", NA))
  expect_identical(attr(g, "forest_recipe")$labs$xlab, list("A", NULL))
  xaxis <- g$grobs[[which(g$layout$name == "xaxis-3")]]
  expect_null(xaxis$children$xlab)

  expect_error(set_labs(p, xlab = c("A", "B", "C")),
               "xlab must be of length 1 or the same length as ci_column")
  expect_error(set_labs(p, title = c("A", "B")), "title must be of length 1")

  # Arrows for all columns, for each column, or removed
  g <- set_labs(p, arrow = c("L", "R"))
  expect_true(all(c("arrow-2", "arrow-3") %in% g$layout$name))

  g <- set_labs(p, arrow = list(NA, c("L", "R")))
  expect_true("arrow-3" %in% g$layout$name)
  expect_false("arrow-2" %in% g$layout$name)

  g <- set_labs(g, arrow = NULL)
  expect_same_svg(g, p)

  expect_error(set_labs(p, arrow = "L"), "Arrow label must be of length 2")
  expect_error(set_labs(p, arrow = list(c("L", "R"))),
               "arrow must have the same length as ci_column")
  expect_error(set_labs(p, arrow = list(c("L", "R"), "L")),
               "Elements in the arrow must be of length 2")

  # Legend text, NULL goes back to the default
  g <- set_labs(p, legend_labels = c("A", "B"), legend_title = "Trial")
  theme <- resolve_theme(attr(g, "forest_recipe"))
  expect_identical(theme$legend$label, c("A", "B"))
  expect_identical(theme$legend$name, "Trial")
  expect_true("legend" %in% g$layout$name)

  expect_same_svg(set_labs(g, legend_labels = NULL, legend_title = NULL), p)
})


test_that("The x-axis is kept, replaced and reset", {

  p <- one_col()

  # Settings left out are kept
  g <- set_xaxis(p, xlim = c(0, 5), vline = 2)
  g <- set_xaxis(g, ticks_at = c(1, 2, 4), ticks_digits = 1L)
  axis <- attr(g, "forest_recipe")$xaxis
  expect_identical(axis$xlim, list(c(0, 5)))
  expect_identical(axis$vline, list(2))
  expect_identical(axis$ticks_at, list(c(1, 2, 4)))
  expect_identical(axis$ticks_digits, list(1L))
  expect_true("vert.line-4" %in% g$layout$name)

  # NULL goes back to the default
  g <- set_xaxis(g, xlim = NULL, ticks_at = NULL, ticks_digits = NULL, vline = NULL)
  expect_same_svg(g, p)

  # The reference line follows the scale unless it is given
  g <- forest(dt[, c(1:3, 19)],
              est = dt$est,
              lower = dt$low,
              upper = dt$hi,
              ci_column = 4)
  expect_same_svg(set_xaxis(g, x_trans = "log"),
                  set_xaxis(p, x_trans = "log"))
  expect_same_svg(set_xaxis(set_xaxis(g, x_trans = "log"), x_trans = NULL),
                  forest(dt[, c(1:3, 19)],
                         est = dt$est,
                         lower = dt$low,
                         upper = dt$hi,
                         ci_column = 4,
                         ref_line = 0))

  # One setting for each column, NA leaves a column at its default
  p <- two_col()
  g <- set_xaxis(p,
                 xlim = list(c(0, 6), NA),
                 ticks_digits = c(1, NA),
                 x_trans = c("log", NA),
                 vline = list(NA, c(2, 3)))
  axis <- attr(g, "forest_recipe")$xaxis
  expect_identical(axis$xlim, list(c(0, 6), NULL))
  expect_identical(axis$ticks_digits, list(1, NULL))
  expect_identical(axis$x_trans, c("log", "none"))
  expect_identical(axis$vline, list(NULL, c(2, 3)))
  expect_true("vert.line-3" %in% g$layout$name)
  expect_false("vert.line-2" %in% g$layout$name)

  g <- set_xaxis(g, xlim = NULL, ticks_digits = NULL, x_trans = NULL, vline = NULL)
  expect_same_svg(g, p)

  # Missing arguments passed on by a wrapper are left out as well
  wrapper <- function(plot, xlim, vline) set_xaxis(plot, xlim = xlim, vline = vline)
  g <- wrapper(set_xaxis(p, vline = 2), xlim = c(0, 6))
  axis <- attr(g, "forest_recipe")$xaxis
  expect_identical(axis$vline, list(2, 2))
  expect_identical(axis$xlim, list(c(0, 6), c(0, 6)))
})


test_that("The x-axis settings are checked", {

  p <- one_col()
  p2 <- two_col()

  expect_error(set_xaxis(p, xlim = c(3, 1)),
               "xlim must be numeric and of length 2")
  expect_error(set_xaxis(p, xlim = 1), "xlim must be numeric and of length 2")
  expect_error(set_xaxis(p2, xlim = list(c(0, 1))),
               "xlim must have the same length as ci_column")
  expect_error(set_xaxis(p2, xlim = list(c(0, 1), "a")),
               "The elements in xlim must be numeric")
  expect_error(set_xaxis(p, ticks_at = "a"), "ticks_at must be numeric")
  expect_error(set_xaxis(p, ticks_minor = "a"), "ticks_minor must be numeric")
  expect_error(set_xaxis(p2, ticks_digits = c(1, 2, 3)),
               "ticks_digits must be length of 1 or same length as ci_column")
  expect_error(set_xaxis(p, ticks_digits = "a"), "ticks_digits must be numeric")
  expect_error(set_xaxis(p, x_trans = "sqrt"), "x_trans must be in")
  expect_error(set_xaxis(p2, x_trans = c("log", "log", "log")),
               "x_trans must be in")
  expect_error(set_xaxis(p, vline = "a"), "vline must be numeric")
  expect_error(set_xaxis(p, x_trans = "log", vline = 0),
               "should be larger than 0")

  # Ticks are checked against the limits, also when set in two calls
  expect_warning(set_xaxis(p, xlim = c(0, 5), ticks_at = c(1, 6)),
                 "ticks_at is outside the xlim")
  g <- set_xaxis(p, ticks_minor = c(1, 6))
  expect_warning(set_xaxis(g, xlim = c(0, 5)),
                 "ticks_minor is outside the xlim")
})


test_that("Old arguments of forest() still work with a message", {

  rm(list = ls(superseded_seen), envir = superseded_seen)

  msg <- messages(p <- one_col(arrow_lab = c("L", "R"),
                               title = "Title",
                               x_trans = "log",
                               vert_line = 2))
  expect_identical(msg,
                   c("arrow_lab, title will be deprecated, use set_labs() instead.\n",
                     "x_trans, vert_line will be deprecated, use set_xaxis() instead.\n"))

  # Once per session
  expect_length(messages(one_col(title = "Title", x_trans = "log")), 0)
  expect_identical(messages(one_col(footnote = "Note", xlab = "HR")),
                   "footnote, xlab will be deprecated, use set_labs() instead.\n")
  expect_identical(messages(one_col(xlim = c(0, 5), ticks_at = 1,
                                    ticks_digits = 1, ticks_minor = 1)),
                   "xlim, ticks_at, ticks_digits, ticks_minor will be deprecated, use set_xaxis() instead.\n")

  # `theme` is now `style`
  expect_identical(messages(one_col(theme = forest_style(base_size = 10))),
                   "theme will be deprecated, use style of forest() instead.
")
  expect_same_svg(suppressMessages(one_col(theme = forest_style(base_size = 10))),
                  one_col(style = forest_style(base_size = 10)))
  expect_error(suppressMessages(one_col(style = forest_style(), theme = forest_style())),
               "not to both `style` and `theme`")

  # Same plot as the new functions, and they do not reach `fn_ci`
  g <- one_col() |>
    set_xaxis(x_trans = "log", vline = 2) |>
    set_labs(arrow = c("L", "R"), title = "Title")
  expect_same_svg(g, p)
  expect_named(attr(p, "forest_recipe")$ci$dots, character())

  # The plots can be updated with the new functions
  expect_same_svg(set_xaxis(p, vline = NA),
                  set_xaxis(g, vline = NA))

  # Other arguments still go to `fn_ci`
  p <- one_col(gp = gpar(lwd = 2))
  expect_identical(attr(p, "forest_recipe")$ci$dots, list(gp = gpar(lwd = 2)))

  # Checks of the old arguments are kept
  expect_error(suppressMessages(one_col(arrow_lab = "L")),
               "Arrow label must be of length 2")
  expect_error(suppressMessages(one_col(title = c("A", "B"))),
               "title must be of length 1")
  expect_error(suppressMessages(one_col(xlim = c(3, 1))),
               "xlim must be numeric and of length 2")
  expect_error(suppressMessages(one_col(x_trans = "sqrt")),
               "x_trans must be in")

  # The same checks as `set_xaxis()`
  expect_error(suppressMessages(two_col(ticks_at = list(1, "a"))),
               "The elements in ticks_at must be numeric")
  expect_error(suppressMessages(one_col(vert_line = "a")),
               "vline must be numeric")
  expect_warning(suppressMessages(two_col(xlim = list(c(0, 6), c(0, 6)),
                                          ticks_at = list(1, c(1, 8)))),
                 "ticks_at is outside the xlim")
})


test_that("Arguments that would be dropped are reported", {

  expect_error(one_col(xlims = c(0, 4)),
               "Unknown arguments: `xlims`. They are not used by `fn_ci`")
  expect_error(one_col(vertline = 2, foo = 1), "`vertline`, `foo`")

  # Arguments of the drawing functions and of `index_args` are kept
  expect_no_error(one_col(gp = gpar(lwd = 2)))
  expect_no_error(forest(dt[, c(1:3, 19)],
                         est = dt$est,
                         lower = dt$low,
                         upper = dt$hi,
                         ci_column = 4,
                         fn_ci = make_boxplot,
                         lowhinge = dt$low,
                         uphinge = dt$hi,
                         index_args = c("lowhinge", "uphinge")))
})


test_that("Messages about the axis are given again when it changes", {

  p <- one_col()
  expect_message(g <- set_xaxis(p, xlim = c(2.5, 5)),
                 "The confidence interval of row")

  # The axis changed, so the message is about the new one
  expect_message(g2 <- set_xaxis(g, xlim = c(2.6, 5)),
                 "The confidence interval of row")

  # Other functions do not repeat it
  expect_silent(set_labs(g2, title = "Title"))

  # It stops once the limits cover the intervals
  expect_silent(set_xaxis(g2, xlim = c(0, 5)))
})


test_that("Legend labels are given for each group", {

  p <- two_col()
  expect_error(set_labs(p, legend_labels = c("A", "B", "C")),
               "legend_labels should be provided for each group")
  expect_no_error(set_labs(p, legend_labels = c("A", "B")))
})


test_that("Size warnings are given when drawn and rebuilt plots warn again", {

  p <- one_col(sizes = 5)
  expect_warning(draw_null(p), "outside the usual range")
  expect_no_warning(draw_null(p))

  # Edits keep the plot, rebuilds give a new one
  expect_no_warning(draw_null(edit_plot(p, row = 1, gp = gpar(col = "red"))))
  expect_warning(draw_null(set_labs(p, title = "Title")), "outside the usual range")
})
