
#' Forest plot
#'
#' @param data Data to be displayed in the forest plot
#' @param est Point estimation. Can be a list for multiple columns
#' and/or multiple groups. If the length of the list is larger than
#' then length of \code{ci.column}, then the values reused for each column
#' and considered as different groups.
#' @param lower Lower bound of the confidence interval, same as \code{est}.
#' @param upper Upper bound of the confidence interval, same as \code{est}.
#' @param sizes Size of the point estimation box, can be a unit, vector or a list.
#' @param ref.line X-axis coordinates of zero line, default is 1.
#' @param ci.column Column number of the data the CI will be displayed.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high). It
#' will take the maximum and minimum of the \code{tick.breaks} or CI.
#' @param tick.breaks X-axis breaks points, a vector.
#' @param arrow.lab Labels for the arrows, string vector of length two (left and
#' right). The theme of arrow will inherit from the x-axis.
#' @param legend A list of legend parameters, see \code{\link{set_legend}}.
#' @param footnote Footnote for the forest plot, will be aligned at left bottom
#' of the plot. Please adjust the line length with line break to avoid the overlap
#' with the arrow and/or x-axis.
#' @param nudge_y Horizontal adjustment to nudge groups by, must be within 0 to 1.
#' @param theme Theme of the forest plot, see \code{\link{forest_theme}} for
#' details.
#'
#' @return A gtable object.
#' @export
#'
#'

forest <- function(data,
                   est,
                   lower,
                   upper,
                   sizes = 0.4,
                   ref.line = 1,
                   ci.column,
                   xlim = NULL,
                   tick.breaks = NULL,
                   arrow.lab = NULL,
                   legend = NULL,
                   footnote = NULL,
                   nudge_y = 0,
                   theme = NULL){

  # Point sizes
  if(length(sizes) == 1 & !inherits(sizes, "list"))
    sizes <- rep(sizes, nrow(data))

  # Check arrow
  if(!is.null(arrow.lab) & length(arrow.lab) != 2)
    stop("Arrow label must of length 2.")

  # Set theme
  if(is.null(theme)){
    theme <- forest_theme()
  }

  # Check length
  if(length(unique(c(length(est), length(lower), length(upper)))) != 1)
      stop("Estimate, lower and upper should have the same length.")

  if(inherits(est, "list") | inherits(lower, "list") | inherits(upper, "list")){

    if(!inherits(est, "list") | !inherits(lower, "list") | !inherits(upper, "list"))
      stop("Estimate, lower and upper must be a list if plan to plot CI for multiple columns.")

    # Calculate group number
    group_num <- length(est)/length(ci.column)
    ci_col_list <- rep(ci.column, group_num)

    # Replicate sizes to align with est and CIs
    if(!inherits(sizes, "list"))
      sizes <- rep(list(sizes), length(ci_col_list))

    # check legend
    if(!is.null(legend) & !inherits(legend, "ciset"))
      stop("Legend must be set with 'set_legend'.")

    # If color is given and not have the same length as group number
    if(group_num > 1 & is.null(legend))
      legend <- .gen_legend(group_num)

    # Get color and pch
    color_list <- rep(legend$color, each = length(ci.column))
    pch_list <- rep(legend$pch, each = length(ci.column))

    # Check nudge_y
    if(nudge_y >= 1 || nudge_y < 0)
      stop("`nudge_y` must be within 0 to 1.")

    # Check nudge_y
    if(group_num > 1 & nudge_y == 0)
      nudge_y <- 0.1

    # Create nudge_y vector
    if(group_num > 1){
      if((group_num %% 2) == 0){
        rep_tm <- cumsum(c(nudge_y/2, rep(nudge_y, group_num)))
        nudge_y <- c(rep_tm[1:(group_num/2)], -rep_tm[1:(group_num/2)])
      }else{
        rep_tm <- cumsum(c(0, rep(nudge_y, group_num)))
        nudge_y <- c(0, rep_tm[2:(group_num/2)], -rep_tm[2:(group_num/2)])
      }
    }

    nudge_y <- rep(nudge_y, each = length(ci.column))

  }else{
    if(length(unique(c(length(est), length(lower), length(upper), nrow(data)))) != 1)
      stop("Estimate, lower and upper should be same length as data row number.")

    if(!is.null(sizes) & length(sizes) != 1){
      sizes <- sqrt(1/sizes)
      sizes <- sizes/max(sizes, na.rm = TRUE)
    }

    ci_col_list <- ci.column
    color_list <- theme$ci$col
    pch_list <- theme$ci$pch

    group_num <- 1

    est <- list(est)
    lower <- list(lower)
    upper <- list(upper)
    sizes <- list(sizes)

  }

  # Check xlim
  if(is.null(xlim) & is.null(tick.breaks)){
    xlim <- range(c(lower, upper), na.rm = TRUE)
    xlim <- c(floor(xlim[1]), ceiling(xlim[2]))
  }


  if(is.null(xlim) & !is.null(tick.breaks))
    xlim <- range(tick.breaks)

  # X-axis breaks
  if(is.null(tick.breaks))
    tick.breaks <- c(xlim[1], ref.line, xlim[2])


  # Calculate heights
  col_height <- apply(data,
                      1,
                      function(x){
                        max(convertHeight(stringHeight(x), "mm",
                                         valueOnly = TRUE))
                      })
  col_height <- unit(col_height, "mm")

  # Add increase heights for multiple groups
  if(group_num > 1){
    heights <- group_num*0.7*col_height + theme$tab_theme$core$padding[2]
    # Convert data to plot
    gt <- tableGrob(data,
                    theme = theme$tab_theme,
                    heights = heights,
                    rows = NULL)
  }else{
    gt <- tableGrob(data,
                    theme = theme$tab_theme,
                    rows = NULL)
  }


  # Draw CI
  for(col_num in seq_along(ci_col_list)){

    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next
      draw_ci <- makeci(est = est[[col_num]][i],
                        lower = lower[[col_num]][i],
                        upper = upper[[col_num]][i],
                        size = sizes[[col_num]][i],
                        xlim = xlim,
                        pch = pch_list[col_num],
                        nudge_y = nudge_y[col_num],
                        color = color_list[col_num])

      gt <- gtable_add_grob(gt, draw_ci,
                            t = i + 1,
                            l = ci_col_list[col_num],
                            b = i + 1,
                            r = ci_col_list[col_num],
                            clip = "off",
                            name = paste0("ci-", i, "-", col_num))
    }
  }

  tot_row <- nrow(gt)

  # X axis
  x_axis <- make_xais(at = tick.breaks, gp = theme$xaxis, xlim = xlim)

  x_axht <- sum(grobHeight(x_axis$children)) + unit(.5, "lines")
  gt <- gtable_add_rows(gt, heights = convertHeight(x_axht, "mm"))

  # Add footnote
  if(!is.null(footnote)){
    footnote_grob <- textGrob(label = footnote,
                              gp = theme$footnote,
                              x = 0,
                              just = c("left", "top"),
                              check.overlap = TRUE,
                              name = "footnote")

    gt <- gtable_add_grob(gt,
                          footnote_grob,
                          t = tot_row + 1,
                          l = 1,
                          b = tot_row + 1, r = min(ci.column),
                          clip = "off",
                          name = "footnote")
  }

  # Add arrow
  if(!is.null(arrow.lab)){
    arrow_grob <- make_arrow(x0 = ref.line,
                            arrow.lab = arrow.lab,
                            gp = theme$xaxis,
                            xlim = xlim)

    lb_ht <- max(grobHeight(arrow_grob$children)) + unit(.5, "lines")
    gt <- gtable_add_rows(gt, heights = convertHeight(lb_ht, "mm"))

  }

  for(j in ci.column){
    # Add reference line
    gt <- gtable_add_grob(gt,
                          vert_line(x = ref.line, gp = theme$refline,
                                    xlim = xlim),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          clip = "off",
                          name = paste0("reference.line-", j))

    # Add row for the X-axis
    gt <- gtable_add_grob(gt, x_axis,
                          t = tot_row + 1,
                          l = j,
                          b = tot_row + 1, r = j,
                          clip = "off",
                          name = paste0("xaxis-", j))
    # # Add arrow
    if(!is.null(arrow.lab))
      gt <- gtable_add_grob(gt, arrow_grob,
                            t = nrow(gt), l = j,
                            b = nrow(gt), r = j,
                            clip = "off",
                            name = paste0("arrow-", j))

  }

  # Add legend
  if(group_num > 1){

    by_row <- if(!legend$position %in% c("top", "bottom") || is.null(legend$position)) TRUE else FALSE

    legend$fontsize <- theme$legend$fontsize
    legend$fontfamily <- theme$legend$fontfamily

    leg_grob <- do.call(legend_grob, legend)

    if(by_row){
      gt <- gtable_add_cols(gt, widths = max(grobWidth(leg_grob$children)) + unit(.5, "lines"))
      gt <- gtable_add_grob(gt, leg_grob,
                            t = 2, l = ncol(gt),
                            b = nrow(gt)-1, r = ncol(gt),
                            clip = "off",
                            name = "legend")
    }else{
      add_pos <- ifelse(legend$position == "top", 0, -1)
      gt <- gtable_add_rows(gt, heights = max(grobHeight(leg_grob$children)) + unit(.5, "lines"), pos = add_pos)
      gt <- gtable_add_grob(gt, leg_grob,
                            t = if(add_pos == 0) 1 else nrow(gt), l = 1,
                            b = if(add_pos == 0) 1 else nrow(gt), r = ncol(gt),
                            clip = "off",
                            name = "legend")
    }
  }

  # Add padding
  gt <- gtable_add_padding(gt, unit(1, "cm"))

  # Auto fit the page
  # gt$widths <- unit(rep(1/ncol(gt), ncol(gt)), "npc")
  # gt$heights <- unit(rep(1/nrow(gt), nrow(gt)), "npc")

  class(gt) <- union("forestplot", class(gt))

  attributes(gt)$data.dim <- dim(data)

  return(gt)

}


# plot
#' @export
plot.forestplot <- function(x,...){
  grid.newpage()
  grid.draw(x)
}

#' @export
print.forestplot <- plot.forestplot
