


#' Forest plot default theme
#'
#' Default theme for the forest plot, but can pass other parameters. The
#' parameters will be passed to corresponding elements of the forest plot.
#' See \code{\link[grid]{gpar}} for details.
#'
#' @param base_size The size of text
#' @param base_family The font family
#' @param xaxis_lwd Line width for x-axis.
#' @param xaxis_cex Multiplier applied to font size for x-axis.
#' @param refline_lwd Line width for reference line.
#' @param refline_lty Line type for reference line.
#' @param refline_col Line color for the reference line.
#' @param footnote_cex Multiplier applied to font size for footnote.
#' @param footnote_fontface The font face for footnote.
#' @param footnote_col Color of the footnote.
#' @param ... Other parameters passed to table. See \code{\link[gridExtra]{tableGrob}}
#'  for details.
#'
#' @export
#'
forest_theme <- function(base_size=12,
                         base_family = "",
                         # X-axis
                         xaxis_lwd = 0.6,
                         xaxis_cex = 1,
                         # Reference line
                         refline_lwd = 1,
                         refline_lty = "dashed",
                         refline_col = "grey20",
                         # Footnote
                         footnote_cex = 0.6,
                         footnote_fontface = "plain",
                         footnote_col = "black",
                         # Legend
                         # legend_lwd = 0.6,
                         ...){

    # Reference line
    refline_gp <- gpar(lwd = refline_lwd,
                       lty = refline_lty,
                       col = refline_col,
                       fontsize = base_size,
                       fontfamily = base_family)

    # X-axis
    xaxis_gp <- gpar(lwd = xaxis_lwd,
                     fontsize = base_size,
                     fontfamily = base_family)

    # Footnote
    footnote_gp <- gpar(fontsize = base_size,
                        fontfamily = base_family,
                        cex = footnote_cex,
                        fontface = footnote_fontface,
                        col = footnote_col)

    # Legend
    legend_gp <- gpar(fontsize = base_size,
                      fontfamily = base_family)

    core <- list(fg_params = list(hjust = 0,
                               x = 0.05,
                               fontsize = base_size,
                               fontfamily = base_family),
              bg_params = list(fill=c(rep(c("#eff3f2", "white"),
                                length.out=4))),
              padding = unit(c(4, 3), "mm"))

    colhead <- list(fg_params = list(hjust = 0, x = 0.05,
                                    fontface=2L,
                                    fontsize = base_size,
                                    fontfamily = base_family),
                   bg_params = list(fill = "white"),
                   padding = unit(c(4, 4), "mm"))

    default <- list(core = core,
                    colhead = colhead)

    tab_theme <- modifyList(default, list(...))
    tab_theme <- modifyList(ttheme_minimal(), tab_theme)

    return(list(legend = legend_gp,
                xaxis = xaxis_gp,
                footnote = footnote_gp,
                refline = refline_gp,
                tab_theme  = tab_theme))

}

