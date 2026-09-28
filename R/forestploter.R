#' forestploter: create a flexible forest plot
#'
#' The layout of the forest plot is the layout of the data given to
#' \code{\link{forest}}, which draws the table and the confidence intervals.
#' The other parts are added with functions that take the plot as their first
#' argument, so they can be chained with the pipe \code{|>}:
#'
#' \itemize{
#'   \item \code{\link{set_xaxis}} Limits, tick marks and scale of the x-axis,
#'   and vertical lines
#'   \item \code{\link{set_labs}} Title, x-axis labels, footnote, arrow labels
#'   and legend labels
#'   \item \code{\link{scale_sizes}} Point sizes scaled by study weights
#'   \item \code{\link{set_style}} Graphical parameters, see
#'   \code{\link{forest_style}}
#' }
#'
#' The plot is a \code{\link[gtable]{gtable}} at every step, so it can be drawn,
#' saved with \code{ggplot2::ggsave} or combined with other plots. Afterwards it
#' can be edited cell by cell with \code{\link{edit_plot}},
#' \code{\link{add_text}}, \code{\link{insert_text}}, \code{\link{add_border}}
#' and \code{\link{add_grob}}.
#'
#' The vignettes \code{vignette("forestploter-intro")} and
#' \code{vignette("forestploter-post")} walk through both steps.
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import grid
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_padding gtable_add_cols
#' @importFrom gridExtra tableGrob ttheme_minimal
## usethis namespace: end
NULL
