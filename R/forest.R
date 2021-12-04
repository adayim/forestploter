
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
#' @param ci.column.width Width of the CI column in the forest plot, default is 1. This
#' is the ratio of maximum width of the columns in the data.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high). It
#' will take the maximum and minimum of the \code{tick.breaks} or CI.
#' @param tick.breaks X-axis breaks points, a vector.
#' @param arrow.lab Labels for the arrows, string vector of length two (left and
#' right).
#' @param ci.color A named vector, name will be used for goup name and color for
#' the CI.
#' @param legend A list of legend parameters. To be passed to \code{\link{legend_grob}}.
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
                   ci.column.width = 2,
                   xlim = NULL,
                   tick.breaks = NULL,
                   arrow.lab = NULL,
                   ci.color = "black",
                   legend = list(name = NULL, position = NULL),
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

    # If color is given and not have the same length as group number
    if(group_num > 1 & length(ci.color) != 1 & group_num != length(ci.color))
      stop("Color should have the same length as group number.")

    # If only one color is given for multiple groups
    if(length(ci.color) == 1){
      color_list <- rep(ci.color, length(ci_col_list))
    }else{
      color_list <- rep(ci.color, each = length(ci.column))
    }

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
    color_list <- ci.color

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


  # Calculate width of the table and multiple ratio of the CI column
  tmp_tab <- tableGrob(data, theme = theme$tab_theme, rows = NULL)
  col_width <- tmp_tab$widths
  col_width[ci.column] <- ci.column.width*col_width[ci.column]

  # Convert data to plot
  gt <- tableGrob(data,
                  theme = theme$tab_theme,
                  rows = NULL,
                  width = col_width)

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
                        nudge_y = nudge_y[col_num],
                        color = color_list[col_num])

      gt <- gtable_add_grob(gt, draw_ci,
                            t = i + 1,
                            l = ci_col_list[col_num],
                            b = i + 1,
                            r = ci_col_list[col_num],
                            clip = "off",
                            name = paste0("ci.", col_num))
    }
  }

  tot_row <- nrow(gt)

  # X axis
  x_axis <- make_xais(at = tick.breaks, gp = theme$xaxis, xlim = xlim)

  gt <- gtable_add_rows(gt, heights = convertHeight(max(grobHeight(x_axis$children)), "mm"))

  # Add arrow
  if(!is.null(arrow.lab)){
    arrow_grob <- make_arrow(x0 = ref.line,
                            arrow.lab = arrow.lab,
                            gp = theme$xaxis,
                            xlim = xlim)

    arrow_lab_height <- sum(convertHeight(stringHeight(arrow.lab), "mm"))

    gt <- gtable_add_rows(gt, heights = arrow_lab_height)

  }

  for(j in ci.column){
    # Add reference line
    gt <- gtable_add_grob(gt,
                          vert_line(x = ref.line, gp = theme$refline,
                                    xlim = xlim),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          clip = "off")

    # Add row for the X-axis
    gt <- gtable_add_grob(gt, x_axis,
                          t = tot_row + 1,
                          l = j,
                          b = tot_row + 1, r = j,
                          clip = "off",
                          name = "xaxis")
    # # Add arrow
    if(!is.null(arrow.lab))
      gt <- gtable_add_grob(gt, arrow_grob,
                            t = nrow(gt), l = j,
                            b = nrow(gt), r = j,
                            clip = "off")

  }

  # Add legend
  if(length(ci.color) > 1){

    by_row <- if(!legend$position %in% c("top", "bottom") || is.null(legend$position)) TRUE else FALSE

    legend$color <- ci.color
    legend$fontsize <- theme$legend$fontsize
    legend$fontfamily <- theme$legend$fontfamily

    leg_grob <- do.call(legend_grob, legend)

    if(by_row){
      gt <- gtable_add_cols(gt, widths = grobWidth(leg_grob))
      gt <- gtable_add_grob(gt, leg_grob,
                            t = 2, l = ncol(gt),
                            b = nrow(gt)-1, r = ncol(gt),
                            clip = "off")
    }else{
      add_pos <- ifelse(legend$position == "top", 0, -1)
      gt <- gtable_add_rows(gt, heights = grobHeight(leg_grob), pos = add_pos)
      gt <- gtable_add_grob(gt, leg_grob,
                            t = if(add_pos == 0) 1 else nrow(gt), l = 1,
                            b = if(add_pos == 0) 1 else nrow(gt), r = ncol(gt),
                            clip = "off")
    }
  }

  # Add padding
  gt <- gtable_add_padding(gt, unit(1, "cm"))

  # Auto fit the page
  gt$widths <- unit(rep(1/ncol(gt), ncol(gt)), "npc")
  gt$heights <- unit(rep(1/nrow(gt), nrow(gt)), "npc")

  class(gt) <- union("forestplot", class(gt))

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
