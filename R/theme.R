
#' Forest plot default theme
#'
#' Default theme for the forest plot, but can pass other parameters. The
#' parameters will be passed to corresponding elements of the forest plot.
#' See \code{\link[grid]{gpar}} for details.
#'
#' @param base_size The size of text
#' @param base_family The font family
#' @param ci_pch Shape of the point estimation. It will be reused if the
#' forest plot is grouped.
#' @param ci_col Color of the CI. A vector of color should be provided for
#' the grouped forest plot. An internal color set will be if only not.
#' @param legend_name Title of the legend.
#' @param legend_position Position of the legend, \code{"right"}, \code{"top"},
#' \code{"bottom"}.
#' @param legend_value Legend labels (expressions). A vector should be provided
#' for the grouped forest plot. A "Group 1" etc will be created if not a vector
#' for a grouped forest plot.
#' @param xaxis_lwd Line width for x-axis.
#' @param xaxis_cex Multiplier applied to font size for x-axis.
#' @param refline_lwd Line width for reference line.
#' @param refline_lty Line type for reference line.
#' @param refline_col Line color for the reference line.
#' @param vertline_lwd Line width for extra vertical line. A vector can be provided
#' for each vertical line, and the values will be recycled if no enough values are
#' given.
#' @param vertline_lty Line type for extra vertical line. Works same as \code{vertline_lwd}.
#' @param vertline_col Line color for the extra vertical line. Works same as \code{vertline_lwd}.
#' @param footnote_cex Multiplier applied to font size for footnote.
#' @param footnote_fontface The font face for footnote.
#' @param footnote_col Color of the footnote.
#' @param ... Other parameters passed to table. See \code{\link[gridExtra]{tableGrob}}
#'  for details.
#'
#' @importFrom utils modifyList
#' 
#' @return A list.
#'
#' @export
#'
forest_theme <- function(base_size=12,
                         base_family = "",
                         # Confidence interval
                         ci_pch = 15,
                         ci_col = "black",
                         # Legend
                         legend_name = "Group",
                         legend_position = "right",
                         legend_value = "",
                         # X-axis
                         xaxis_lwd = 0.6,
                         xaxis_cex = 1,
                         # Reference line
                         refline_lwd = 1,
                         refline_lty = "dashed",
                         refline_col = "grey20",
                         # Vertical line
                         vertline_lwd = 1,
                         vertline_lty = "dashed",
                         vertline_col = "grey20",
                         # Footnote
                         footnote_cex = 0.6,
                         footnote_fontface = "plain",
                         footnote_col = "black",
                         # Legend
                         # legend_lwd = 0.6,
                         ...){
    
    legend_position <- match.arg(legend_position, c("right", "top", "bottom"))  

    # Default color set
    col_set <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
                "#ffff33","#a65628","#f781bf","#999999")

    # Recycle if one of the values
    max_len <- list(legend_value, ci_pch, ci_col)
    max_len <- max(vapply(max_len, length, FUN.VALUE = 1L), na.rm = TRUE) 
    
    if(max_len > 1){
      if(length(legend_value) < max_len)
        stop("legend_value should be provided each groups.")

      ci_pch <- rep_len(ci_pch, max_len)
      if(length(ci_col) == 1)
        ci_col <- col_set[1:max_len] 

    }
                       

    # Reference line
    refline_gp <- gpar(lwd = refline_lwd,
                       lty = refline_lty,
                       col = refline_col,
                       fontsize = base_size,
                       fontfamily = base_family)

    # Reference line
    vertline_gp <- gpar(lwd = vertline_lwd,
                        lty = vertline_lty,
                        col = vertline_col,
                        fontsize = base_size,
                        fontfamily = base_family)

    # Confidence interval
    ci_gp <- list(pch = ci_pch, col = ci_col)

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
    legend_gp <- list(fontsize = base_size,
                      fontfamily = base_family,
                      name = legend_name,
                      position = legend_position,
                      label = legend_value)

    # Table body
    core <- list(fg_params = list(hjust = 0,
                               x = 0.05,
                               fontsize = base_size,
                               fontfamily = base_family),
              bg_params = list(fill=c(rep(c("#eff3f2", "white"),
                                length.out=4))),
              padding = unit(c(4, 3), "mm"))

    # Table header
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
                ci = ci_gp,
                xaxis = xaxis_gp,
                footnote = footnote_gp,
                refline = refline_gp,
                vertline = vertline_gp,
                tab_theme  = tab_theme))

}

