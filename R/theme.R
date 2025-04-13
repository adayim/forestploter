
#' Forest plot default theme
#'
#' @description
#'
#' Default theme for the forest plot, but can pass other parameters. The
#' parameters will be passed to corresponding elements of the forest plot.
#'
#' \itemize{
#'   \item \code{ci_*} Control the graphical parameters of confidence intervals
#'   \item \code{legend_*} Control the graphical parameters of legend
#'   \item \code{xaxis_*} Control the graphical parameters of x-axis
#'   \item \code{refline_*} Control the graphical parameters of reference line
#'   \item \code{vertline_*} Control the graphical parameters of vertical line
#'   \item \code{summary_*} Control the graphical parameters of diamond shaped summary CI
#'   \item \code{footnote_*} Control the graphical parameters of footnote
#'   \item \code{title_*} Control the graphical parameters of title
#'   \item \code{arrow_*} Control the graphical parameters of arrow
#' }
#'
#' See \code{\link[grid]{gpar}} for more details.
#'
#' @param base_size The size of text
#' @param base_family The font family
#' @param ci_pch Shape of the point estimation. It will be reused if the
#' forest plot is grouped.
#' @param ci_col Color of the CI. A vector of color should be provided for
#' the grouped forest plot. An internal color set will be if only not.
#' @param ci_fill Color fill the point estimation. A vector of color should be
#'  provided for the grouped forest plot. If this is \code{NULL} (default), the
#' value will inherit from \code{"ci_col"}. This is valid only if
#' \code{ci_pch} within 15:25.
#' @param ci_alpha Scalar value, alpha channel for transparency of point estimation.
#'  A small vertical line will be added to indicate the point estimation if this
#'  is not equals to 1.
#' @param ci_lty Line type of the CI. A vector of line type should be provided
#' for the grouped forest plot.
#' @param ci_lwd Line width of the CI. A vector of line type should be provided
#' for the grouped forest plot.
#' @param ci_Theight A unit specifying the height of the T end of CI. If set to
#' `NULL` (default), no T end will be drawn.
#' @param legend_name Title of the legend.
#' @param legend_position Position of the legend, \code{"right"}, \code{"top"},
#' \code{"bottom"} or \code{"none"} to suppress the legend.
#' @param legend_value Legend labels (expressions). A vector should be provided
#' for the grouped forest plot. A "Group 1" etc will be created if not a vector
#' for a grouped forest plot.
#' @param legend_gp \code{gpar} graphical parameters of legend, see \code{\link[grid]{gpar}}.
#' @param legend_ncol integer; the number of columns, see \code{\link[grid]{legendGrob}}.
#' @param legend_byrow logical indicating whether rows of the legend are filled first, see \code{\link[grid]{legendGrob}}.
#' @param xaxis_gp \code{gpar} graphical parameters of x-axis, see \code{\link[grid]{gpar}}.
#' @param refline_gp \code{gpar} graphical parameters of reference line, see \code{\link[grid]{gpar}}.
#' @param vertline_lwd Line width for extra vertical line. A vector can be provided
#' for each vertical line, and the values will be recycled if no enough values are
#' given.
#' @param vertline_lty Line type for extra vertical line. Works same as \code{vertline_lwd}.
#' @param vertline_col Line color for the extra vertical line. Works same as \code{vertline_lwd}.
#' @param summary_fill Color for filling the summary diamond shape.
#' @param summary_col Color for borders of the summary diamond shape.
#' @param footnote_gp \code{gpar} graphical parameters of footnote, see \code{\link[grid]{gpar}}.
#' @param footnote_parse Parse footnote text (default).
#' @param title_just The justification of the title, default is \code{'left'}.
#' @param title_gp \code{gpar} graphical parameters of title, see \code{\link[grid]{gpar}}.
#' @param arrow_type Type of the arrow below x-axis, see \code{\link[grid]{arrow}}.
#' @param arrow_label_just The justification of the arrow label relative to arrow. Control
#' the arrow label to align to the starting point of the arrow \code{"start"} (default) or
#' the ending point of the arrow \code{"end"}.
#' @param arrow_length The length of the arrow head, default is \code{0.05}.
#' See \code{\link[grid]{arrow}}.
#' @param arrow_gp \code{gpar} graphical parameters of arrow, see \code{\link[grid]{gpar}}.
#' @param xlab_adjust Control the alignment of xlab to reference line (default) or center of the x-axis.
#' @param xlab_gp \code{gpar} graphical parameters of xlab, see \code{\link[grid]{gpar}}.
#' @param ... Other parameters passed to table. See \code{\link[gridExtra]{tableGrob}}
#'  for details.
#'
#'
#' @importFrom utils modifyList
#' @seealso \code{\link[gridExtra]{tableGrob}} \code{\link{forest}} \code{\link[grid]{textGrob}}
#'  \code{\link[grid]{gpar}} \code{\link[grid]{arrow}} \code{\link[grid]{segmentsGrob}}
#' \code{\link[grid]{linesGrob}} \code{\link[grid]{pointsGrob}} \code{\link[grid]{legendGrob}}
#' @return A list.
#'
#' @export
#'
forest_theme <- function(base_size = 12,
                         base_family = "",
                         # Confidence interval
                         ci_pch = 15,
                         ci_col = "black",
                         ci_alpha = 1,
                         ci_fill = NULL,
                         ci_lty = 1,
                         ci_lwd = 1,
                         ci_Theight = NULL,
                         # Legend
                         legend_name = "Group",
                         legend_position = "right",
                         legend_value = "",
                         legend_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 1),
                         legend_ncol = 1,
                         legend_byrow = TRUE,
                         # X-axis
                         xaxis_gp = gpar(fontsize = base_size, fontfamily = base_family, lwd = 0.6, cex = 1),
                         # Reference line
                         refline_gp = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                         # Vertical line
                         vertline_lwd = 1,
                         vertline_lty = "dashed",
                         vertline_col = "grey20",
                         # summary
                         summary_col = "#4575b4",
                         summary_fill = summary_col,
                         # Footnote
                         footnote_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 0.6, fontface = "plain", col = "black"),
                         footnote_parse = TRUE,
                         # Title
                         title_just = c("left", "right", "center"),
                         title_gp = gpar(cex = 1.2, fontface = "bold", col = "black", fontfamily = base_family),
                         # Arrow
                         arrow_type = c("open", "closed"),
                         arrow_label_just = c("start", "end"),
                         arrow_length = 0.05,
                         arrow_gp = gpar(fontsize = base_size, fontfamily = base_family, lwd = 0.6),
                         # legend_lwd = 0.6,
                         # X-lab
                         xlab_adjust = c("refline", "center"),
                         xlab_gp = gpar(fontsize = base_size, fontfamily = base_family, cex = 1, fontface = "plain"),
                         ...){

    dot_args <- list(...)

    legend_position <- match.arg(legend_position, c("right", "top", "bottom", "none"))

    if (!is.unit(arrow_length)) arrow_length <- unit(arrow_length, units = "inches")

    if(!is.null(ci_fill) & !all(ci_pch %in% 15:25))
      warning("`ci_pch` is not within 15:25, `ci_fill` will be ignored.")

    if(length(ci_alpha) > 1)
      stop("`ci_alpha` must be of length 1.")

    # Default color set
    col_set <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
                "#ffff33","#a65628","#f781bf","#999999")

    # Check length
    if(!is.null(ci_Theight) && length(ci_Theight) > 1)
      stop("`ci_Theight` must be of length 1.")

    # Recycle if one of the values
    max_len <- list(legend_value, ci_pch, ci_col, ci_fill, ci_lty, ci_lwd)
    max_len <- max(vapply(max_len, length, FUN.VALUE = 1L), na.rm = TRUE)

    if(max_len > 1){
      if(length(legend_value) < max_len)
        stop("legend_value should be provided each groups.")

      ci_pch <- rep_len(ci_pch, max_len)
      ci_lty <- rep_len(ci_lty, max_len)
      ci_lwd <- rep_len(ci_lwd, max_len)
      ci_alpha <- rep_len(ci_alpha, max_len)

      if(length(ci_col) == 1){
        # Provide color if multiple color and not differentiated with fill
        if(is.null(ci_fill) & max_len > 1)
          ci_col <- col_set[1:max_len]
        else
          ci_col <- rep_len(ci_col, max_len)
      }

      if(is.null(ci_fill)){
        ci_fill <- ci_col
      }else {
        if(length(ci_fill) == 1)
          ci_fill <- rep_len(ci_fill, max_len)
        if(length(ci_fill) > 1 & length(ci_fill) != length(ci_col))
          stop("`ci_fill` must be of length 1 or same length as `ci_col`.")
      }
    }


    # Reference line
    # For backward compatibility
    refline_args <- c("refline_lwd", "refline_lty", "refline_col")
    if(any(names(dot_args) %in% refline_args)){
      message(paste(refline_args[refline_args %in% names(dot_args)], collapse = ", "),
                    " will be deprecated, use refline_gp instead.")
      refline_gp_old <- gpar(lwd = dot_args$refline_lwd,
                             lty = dot_args$refline_lty,
                             col = dot_args$refline_col)
      refline_gp <- modifyList(refline_gp, refline_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% refline_args]
    }
    stop_ifnot_gpar(refline_gp)

    # Reference line
    vertline_gp <- gpar(lwd = vertline_lwd,
                        lty = vertline_lty,
                        col = vertline_col,
                        fontsize = base_size,
                        fontfamily = base_family)

    # Confidence interval
    ci_gp <- list(pch = ci_pch,
                  col = ci_col,
                  fill = ci_fill,
                  lty = ci_lty,
                  alpha = ci_alpha,
                  lwd = ci_lwd,
                  t_height = ci_Theight)

    # X-axis
    # For backward compatability
    xaxis_args <- c("xaxis_lwd", "xaxis_cex")
    if(any(names(dot_args) %in% xaxis_args)){
      message(paste(xaxis_args[xaxis_args %in% names(dot_args)], collapse = ", "),
                    " will be deprecated, use xaxis_gp instead.")
      xaxis_gp_old <- gpar(lwd = dot_args$xaxis_lwd,
                           cex = dot_args$xaxis_cex,
                           fontsize = base_size,
                           fontfamily = base_family)
      xaxis_gp <- modifyList(xaxis_gp, xaxis_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% xaxis_args]
    }
    stop_ifnot_gpar(xaxis_gp)

    # Summary
    sum_gp <- gpar(col = summary_col,
                   fill = summary_fill)

    # Footnote
    # For backward compatability
    footnote_args <- c("footnote_cex", "footnote_fontface", "footnote_col")
    if(any(names(dot_args) %in% footnote_args)){
      message(paste(footnote_args[footnote_args %in% names(dot_args)], collapse = ", "),
                   " will be deprecated, use footnote_gp instead.")
      footnote_gp_old <- gpar(fontsize = base_size,
                              fontfamily = base_family,
                              cex = dot_args$footnote_cex,
                              fontface = dot_args$footnote_fontface,
                              col = dot_args$footnote_col)
      footnote_gp <- modifyList(footnote_gp, footnote_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% footnote_args]
    }
    stop_ifnot_gpar(footnote_gp)
    footnote_gp <- list(gp = footnote_gp,
                        parse = footnote_parse)

    # Legend
    # For backward compatability
    legend_args <- c("legend_cex")
    if(any(names(dot_args) %in% legend_args)){
      message("legend_cex will be deprecated, use legend_gp instead.")
      legend_gp_old <- gpar(fontsize = base_size,
                            fontfamily = base_family,
                            cex = dot_args$legend_cex)
      legend_gp <- modifyList(legend_gp, legend_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% legend_args]
    }
    stop_ifnot_gpar(legend_gp)

    legend_gp <- list(gp = legend_gp,
                      name = legend_name,
                      position = legend_position,
                      label = legend_value,
                      ncol = legend_ncol,
                      byrow = legend_byrow)

    # Title
    # For backward compatability
    title_args <- c("title_cex", "title_fontface", "title_col", "title_fontfamily")
    if(any(names(dot_args) %in% title_args)){
      message(paste(title_args[title_args %in% names(dot_args)], collapse = ", "), 
                    " will be deprecated, use title_gp instead.")
      title_gp_old <- gpar(cex = dot_args$title_cex,
                           fontface = dot_args$title_fontface,
                           col = dot_args$title_col,
                           fontfamily = dot_args$title_fontfamily)
      title_gp <- modifyList(title_gp, title_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% title_args]
    }
    stop_ifnot_gpar(title_gp)
    title_gp <- list(just = match.arg(title_just),
                     gp = title_gp)

    # Arrow
    # For backward compatability
    arrow_args <- c("arrow_lwd", "arrow_fill", "arrow_col", "arrow_cex")
    if(any(names(dot_args) %in% arrow_args)){
      message(paste(arrow_args[arrow_args %in% names(dot_args)], collapse = ", "), 
                    " will be deprecated, use arrow_gp instead.")
      arrow_gp_old <- gpar(fontsize = base_size,
                           fontfamily = base_family,
                           lwd = dot_args$arrow_lwd,
                           fill = dot_args$arrow_fill,
                           col = dot_args$arrow_col,
                           cex = dot_args$arrow_cex)
      arrow_gp <- modifyList(arrow_gp, arrow_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% arrow_args]
    }
    stop_ifnot_gpar(arrow_gp)
    arrow_gp <- list(type = match.arg(arrow_type),
                    label_just = match.arg(arrow_label_just),
                    length = arrow_length,
                    gp = arrow_gp)

    # Arrow
    # For backward compatability
    xlab_args <- c("xlab_fontface", "xlab_cex")
    if(any(names(dot_args) %in% xlab_args)){
      message(paste(xlab_args[xlab_args %in% names(dot_args)], collapse = ", "), 
                    " will be deprecated, use xlab_gp instead.")
      xlab_gp_old <- gpar(fontsize = base_size,
                          fontfamily = base_family,
                          fontface = dot_args$xlab_fontface,
                          cex = dot_args$xlab_cex)
      xlab_gp <- modifyList(xlab_gp, xlab_gp_old)
      dot_args <- dot_args[!names(dot_args) %in% xlab_args]
    }
    stop_ifnot_gpar(xlab_gp)
    xlab_gp <- list(just = match.arg(xlab_adjust),
                    gp = xlab_gp)

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
                                    fontface = 2L,
                                    fontsize = base_size,
                                    fontfamily = base_family),
                   bg_params = list(fill = "white"),
                   padding = unit(c(4, 4), "mm"))

    default <- list(core = core,
                    colhead = colhead)

    tab_theme <- modifyList(default, dot_args)
    tab_theme <- modifyList(ttheme_minimal(), tab_theme)

    return(list(legend = legend_gp,
                ci = ci_gp,
                xaxis = xaxis_gp,
                footnote = footnote_gp,
                title  = title_gp,
                arrow = arrow_gp,
                refline = refline_gp,
                vertline = vertline_gp,
                xlab = xlab_gp,
                summary = sum_gp,
                tab_theme  = tab_theme))

}


#
make_group_theme <- function(theme, group_num){

  # Default color set
  col_set <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
               "#ffff33","#a65628","#f781bf","#999999")

  # If color is given and not have the same length as group number
    if(group_num > 1){
      # If colors for all groups are the same then use default color
      if(length(unique(c(theme$ci$col, theme$ci$fill))) == 1)
        theme$ci$col <- col_set[1:group_num]
      else
        theme$ci$col <- rep_len(theme$ci$col, group_num)
    }

  # If fill is given and not have the same length as group number
    if(group_num > 1 & length(theme$ci$fill) == 1)
      theme$ci$fill <- rep_len(theme$ci$fill, group_num)

  # If alpha is given and not have the same length as group number
    if(group_num > 1 & length(theme$ci$alpha) == 1)
      theme$ci$alpha <- rep_len(theme$ci$alpha, group_num)

    # If line type is given and not have the same length as group number
    if(group_num > 1 & length(theme$ci$lty) == 1)
      theme$ci$lty <- rep_len(theme$ci$lty, group_num)

    # If line width is given and not have the same length as group number
    if(group_num > 1 & length(theme$ci$lwd) == 1)
      theme$ci$lwd <- rep_len(theme$ci$lwd, group_num)

    # Make legend multiple
    if(group_num > 1 & length(theme$ci$pch) == 1)
      theme$ci$pch <- rep_len(theme$ci$pch, group_num)

    if(group_num > 1 && length(theme$legend$label) == 1 && theme$legend$label == ""){
      theme$legend$label <- paste("Group", 1:group_num)
    }

    # Change CI theme if the plot has multiple group
    if(group_num > 1){
      if(length(theme$summary$fill) != length(theme$summary$col))
        stop("summary theme fill and col have different length")

      if(length(theme$summary$col) == 1){
        theme$summary$col <- theme$ci$col
        theme$summary$fill <- theme$ci$col
      }
    }

    # Check for group and color
    if(group_num > 1 & length(theme$ci$col) < group_num & length(theme$ci$col) > 1)
      stop("More groups than colors.")

    # Check for group and legend label
    if(group_num > 1 & length(theme$legend$label) < group_num & length(theme$legend$label) > 1)
      stop("More groups than legend labels.")

    return(theme)

}

