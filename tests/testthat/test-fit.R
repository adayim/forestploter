
#### Prep data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:8, ]
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$` ` <- paste(rep(" ", 20), collapse = " ")
dt$`  ` <- paste(rep(" ", 40), collapse = " ")

one_col <- function(fit = "none", ...){
  forest(dt[, c(1:2, 19)],
         est = dt$est,
         lower = dt$low,
         upper = dt$hi,
         ci_column = 3,
         ref_line = 1,
         style = forest_style(fit = fit, ...)) |>
    set_xaxis(xlim = c(0, 4)) |>
    set_labs(title = "Title", arrow = c("Placebo Better", "Treatment Better"))
}

# Layout of a plot fitted to a page of the given size in mm
fitted <- function(plot, fit, width, height){
  pdf(NULL, width = width / 25.4, height = height / 25.4)
  on.exit(dev.off())
  grid.newpage()
  g <- fit_layout(plot, fit)
  list(plot = g,
       widths = convertWidth(g$widths, "mm", valueOnly = TRUE),
       heights = convertHeight(g$heights, "mm", valueOnly = TRUE),
       nat_w = convertWidth(plot$widths, "mm", valueOnly = TRUE),
       nat_h = convertHeight(plot$heights, "mm", valueOnly = TRUE))
}

# Natural size of a plot in mm
natural <- function(plot){
  pdf(NULL)
  on.exit(dev.off())
  get_wh(plot, unit = "mm")
}

ci_cols <- function(plot){
  sort(unique(plot$layout$l[grepl("^xaxis-", plot$layout$name)]))
}


test_that("CI columns take the free width", {

  p <- one_col()
  nat <- natural(p)
  ci <- ci_cols(p)

  lay <- fitted(p, "width", 2 * nat[["width"]], nat[["height"]])
  expect_equal(sum(lay$widths), 2 * nat[["width"]])
  expect_gt(lay$widths[ci], lay$nat_w[ci])
  expect_equal(lay$widths[-ci], lay$nat_w[-ci])
  expect_equal(lay$heights, lay$nat_h)

  # Tick labels that would overlap are left out
  xaxis <- lay$plot$grobs[[which(lay$plot$layout$name == "xaxis-3")]]
  expect_true(xaxis$children$label$check.overlap)

  # Narrower than the natural width, down to one line of text
  lay <- fitted(p, "width", 0.8 * nat[["width"]], nat[["height"]])
  expect_equal(sum(lay$widths), 0.8 * nat[["width"]])
  expect_lt(lay$widths[ci], lay$nat_w[ci])
  expect_equal(lay$widths[-ci], lay$nat_w[-ci])

  lay <- fitted(p, "width", 10, nat[["height"]])
  pdf(NULL)
  expect_equal(lay$widths[ci], convertWidth(unit(1, "lines"), "mm", valueOnly = TRUE))
  dev.off()
})


test_that("Free width is shared in proportion to the natural width", {

  p <- forest(dt[, c(1, 19, 20)],
              est = list(dt$est_gp1, dt$est_gp2),
              lower = list(dt$low_gp1, dt$low_gp2),
              upper = list(dt$hi_gp1, dt$hi_gp2),
              ci_column = c(2, 3),
              ref_line = 1,
              style = forest_style(fit = "width"))
  nat <- natural(p)
  ci <- ci_cols(p)

  lay <- fitted(p, "width", 1.5 * nat[["width"]], nat[["height"]])
  expect_equal(lay$widths[ci] / lay$nat_w[ci],
               rep(lay$widths[ci[1]] / lay$nat_w[ci[1]], 2))
})


test_that("Rows take the free height with fit = 'both'", {

  p <- one_col()
  nat <- natural(p)
  ci <- ci_cols(p)
  l <- p$layout
  body <- seq(max(l$b[grepl("^colhead-", l$name)]) + 1,
              min(l$t[grepl("^xaxis-", l$name)]) - 1)

  lay <- fitted(p, "both", 2 * nat[["width"]], 1.5 * nat[["height"]])
  expect_equal(sum(lay$widths), 2 * nat[["width"]])
  expect_equal(sum(lay$heights), 1.5 * nat[["height"]])
  added <- lay$heights[body] - lay$nat_h[body]
  expect_equal(added, rep(0.5 * nat[["height"]] / length(body), length(body)))
  expect_equal(lay$heights[-body], lay$nat_h[-body])

  # A page lower than the plot leaves the rows as they are
  lay <- fitted(p, "both", nat[["width"]], 0.5 * nat[["height"]])
  expect_equal(lay$heights, lay$nat_h)
})


test_that("Arrows are laid out for the width of their column", {

  p <- one_col(arrow_label_just = "end")
  nat <- natural(p)

  # The labels only fit next to the column edges in a wide column
  left_just <- function(width){
    g <- fitted(p, "width", width, nat[["height"]])$plot
    g$grobs[[which(g$layout$name == "arrow-3")]]$children$arrow.text.left$just
  }
  expect_identical(left_just(2 * nat[["width"]]), "left")
  expect_identical(left_just(nat[["width"]]), "right")
})


test_that("The fit of the style is used when drawn, edited plots included", {

  p <- one_col(fit = "both")
  nat <- natural(p)

  # The plot keeps its natural size, the fit is applied when drawn
  expect_equal(natural(p), nat)

  context <- function(plot, width, height){
    pdf(NULL, width = width / 25.4, height = height / 25.4)
    on.exit(dev.off())
    grid.newpage()
    g <- makeContext(plot)
    c(width = sum(convertWidth(g$widths, "mm", valueOnly = TRUE)),
      height = sum(convertHeight(g$heights, "mm", valueOnly = TRUE)))
  }
  expect_equal(context(p, 300, 200), c(width = 300, height = 200))
  expect_equal(context(one_col(), 300, 200), nat)

  g <- p |>
    insert_text(text = "Inserted", row = 3) |>
    edit_plot(row = 1, gp = gpar(col = "red")) |>
    add_border(part = "header")
  expect_equal(context(g, 300, 200), c(width = 300, height = 200))

  # Plots can be drawn anywhere
  expect_no_error(draw_null(g))
  expect_no_error(draw_null(gridExtra::arrangeGrob(p, p, ncol = 2)))
})


test_that("autofit gives a message and takes the place of the fit", {

  rm(list = intersect("autofit", ls(superseded_seen)), envir = superseded_seen)
  p <- one_col(fit = "width")

  pdf(NULL)
  on.exit(dev.off())
  expect_message(print(p, autofit = TRUE),
                 "autofit will be deprecated, use fit of forest_style\\(\\) instead.")
  expect_silent(print(p, autofit = TRUE))
  expect_silent(print(p))

  # The page is shared equally, as before
  g <- p
  g$widths <- unit(rep(1/ncol(g), ncol(g)), "npc")
  attr(g, "forest_fit") <- "none"
  expect_identical(makeContext(g)$widths, g$widths)
})


test_that("Fitted plots", {

  vdiffr::expect_doppelganger("fit-width", one_col(fit = "width"))
  vdiffr::expect_doppelganger("fit-both", one_col(fit = "both"))
})
