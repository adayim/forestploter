
# Render a plot to SVG text, the same way as the visual snapshots. The plot is
# built before the SVG device is opened, as some sizes are measured on the
# device that is current when a plot is built.
svg_text <- function(plot){
  force(plot)
  file <- tempfile(fileext = ".svg")
  on.exit(unlink(file), add = TRUE)
  vdiffr::write_svg(plot, file, title = "plot")
  readLines(file, warn = FALSE)
}

# Expect two plots to draw exactly the same
expect_same_svg <- function(object, expected){
  expect_identical(svg_text(object), svg_text(expected))
}

# Draw a plot on a null device
draw_null <- function(plot){
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(plot)
  invisible(plot)
}
