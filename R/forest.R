
#' Forest plot
#'
#' A data frame will be used for the basic layout of the forest plot.
#' Graphical parameters can be set using the \code{\link{forest_theme}} function.
#'
#' @param data Data to be displayed in the forest plot
#' @param est Point estimation. Can be a list for multiple columns
#' and/or multiple groups. If the length of the list is larger than
#' then length of \code{ci_column}, then the values reused for each column
#' and considered as different groups.
#' @param lower Lower bound of the confidence interval, same as \code{est}.
#' @param upper Upper bound of the confidence interval, same as \code{est}.
#' @param sizes Size of the point estimation box, can be a unit, vector or a list.
#' @param ref_line X-axis coordinates of zero line, default is 1. Provide an atomic
#'  vector if different reference line for each \code{ci_column} is desired.
#' @param vert_line Numerical vector, add additional vertical line at given value.
#' Provide a list of numerical vector element if different vertical line for each
#'  \code{ci_column} is desired.
#' @param ci_column Column number of the data the CI will be displayed.
#' @param xlog If TRUE, x-axis tick marks assume values are exponential, e.g.
#' for logistic regression (OR), survival estimates (HR), Poisson regression etc.
#' Provide a logical vector if different conversion for each \code{ci_column} is
#' desired.
#' @param is_summary A logical vector indicating if the value is a summary value,
#' which will have a diamond shape for the estimate. Can not be used with multiple
#' group `forestplot`.
#' @param xlim Limits for the x axis as a vector of length 2, i.e. c(low, high). It
#' will take the minimum and maximum of the lower and upper value if not provided.
#' This will apply to all CI columns if provided, and will be calculated automatically
#' for each column if not provided. This should be a list with the same length of
#' \code{ci_column} if different \code{xlim} for different column is desired.
#' @param ticks_at Set X-axis tick-marks point. This will apply to all CI columns if
#' provided, and will be calculated automatically for each column if not provided.
#' This should be a list if different \code{ticks_at} for different column is desired.
#' @param arrow_lab Labels for the arrows, string vector of length two (left and
#' right). The theme of arrow will inherit from the x-axis. This should be a list
#' if different arrow labels for each column is desired.
#' @param xlab X-axis labels, it will be put under the x-axis. An atomic vector should 
#' be provided if different \code{xlab} for different column is desired.
#' @param footnote Footnote for the forest plot, will be aligned at left bottom
#' of the plot. Please adjust the line length with line break to avoid the overlap
#' with the arrow and/or x-axis.
#' @param nudge_y Horizontal adjustment to nudge groups by, must be within 0 to 1.
#' @param theme Theme of the forest plot, see \code{\link{forest_theme}} for
#' details.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @example inst/examples/forestplot-example.R
#' @export
#'
#'

forest <- function(data,
                   est,
                   lower,
                   upper,
                   sizes = 0.4,
                   ref_line = ifelse(xlog, 1, 0),
                   vert_line = NULL,
                   ci_column,
                   xlog = FALSE,
                   is_summary = NULL,
                   xlim = NULL,
                   ticks_at = NULL,
                   arrow_lab = NULL,
                   xlab = NULL,
                   footnote = NULL,
                   nudge_y = 0,
                   theme = NULL){

  check_errors(data, est, lower, upper, sizes, ref_line, vert_line, ci_column, xlog, is_summary, xlim, ticks_at, arrow_lab, xlab)

  # Point sizes
  if(length(sizes) == 1 & !inherits(sizes, "list"))
    sizes <- rep(sizes, nrow(data))

  # Set theme
  if(is.null(theme)){
    theme <- forest_theme()
  }

  # For multiple ci_column
  if(length(ref_line) == 1)
    ref_line <- rep(ref_line, length(ci_column))

  if(!is.null(vert_line) && !inherits(vert_line, "list"))
    vert_line <- rep(list(vert_line), length(ci_column))

  if(length(xlog) == 1)
    xlog <- rep(xlog, length(ci_column))

  if(!is.null(xlim) && !inherits(xlim, "list"))
    xlim <- rep(list(xlim), length(ci_column))

  if(!is.null(ticks_at) && !inherits(ticks_at, "list"))
    ticks_at <- rep(list(ticks_at), length(ci_column))

  if(!is.null(arrow_lab) && !inherits(arrow_lab, "list"))
    arrow_lab <- rep(list(arrow_lab), length(ci_column))

  if(length(xlab) == 1)
    xlab <- rep(xlab, length(ci_column))

  if(inherits(est, "list") | inherits(lower, "list") | inherits(upper, "list")){

    if(!inherits(est, "list") | !inherits(lower, "list") | !inherits(upper, "list"))
      stop("Estimate, lower and upper must be a list if plan to plot CI for multiple columns.")

    # Calculate group number
    group_num <- length(est)/length(ci_column)
    ci_col_list <- rep(ci_column, group_num)

    # Replicate sizes to align with est and CIs
    if(!inherits(sizes, "list"))
      sizes <- rep(list(sizes), length(ci_col_list))

    theme <- make_group_theme(theme = theme, group_num = group_num)

    # Get color and pch
    color_list <- rep(theme$ci$col, each = length(ci_column))
    pch_list <- rep(theme$ci$pch, each = length(ci_column))
    lty_list <- rep(theme$ci$lty, each = length(ci_column))
    lwd_list <- rep(theme$ci$lwd, each = length(ci_column))

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

    nudge_y <- rep(nudge_y, each = length(ci_column))

  }else{
    if(length(unique(c(length(est), length(lower), length(upper), nrow(data)))) != 1)
      stop("Estimate, lower and upper should be same length as data row number.")

    if(!is.null(sizes) & length(sizes) != 1){
      sizes <- sqrt(1/sizes)
      sizes <- sizes/max(sizes, na.rm = TRUE)
    }

    ci_col_list <- ci_column
    color_list <- theme$ci$col
    pch_list <- theme$ci$pch
    lty_list <- theme$ci$lty
    lwd_list <- theme$ci$lwd

    group_num <- 1

    est <- list(est)
    lower <- list(lower)
    upper <- list(upper)
    sizes <- list(sizes)

  }

  if(group_num > 1 || is.null(is_summary))
    is_summary <- rep(FALSE, nrow(data))

  # Positions of values in ci_column
  gp_list <- rep_len(1:(length(lower)/group_num), length(lower))

  # Check exponential
  for(i in seq_along(xlog)){
    if(xlog[i]){
      sel_num <- gp_list == i
      if (any(unlist(est[sel_num]) <= 0, na.rm = TRUE) ||
            any(unlist(lower[sel_num]) <= 0, na.rm = TRUE) ||
            any(unlist(upper[sel_num]) <= 0, na.rm = TRUE) ||
            (any(ref_line[i] <= 0)) ||
            (!is.null(vert_line) && any(unlist(vert_line[[i]]) <= 0, na.rm = TRUE)) ||
            (!is.null(xlim) && any(unlist(xlim[[i]]) < 0))) {
        stop("est, lower, upper, ref_line, vert_line and xlim should be larger than 0, if `xlog=TRUE`.")
      }
    }
  }

  # Set xlim to minimum and maximum value of the CI
  xlim <- lapply(seq_along(ci_column), function(i){
    sel_num <- gp_list == i
    make_xlim(xlim = xlim[[i]],
              lower = lower[sel_num],
              upper = upper[sel_num],
              ref_line = ref_line[i],
              ticks_at = ticks_at[[i]],
              is_exp = xlog[i])
  })

  # Set X-axis breaks if missing
  ticks_at <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_at[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               is_exp = xlog[i])
  })

  # Calculate heights
  col_height <- apply(data, 1, function(x){
                        max(convertHeight(stringHeight(x), "mm", valueOnly = TRUE))
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

  # Column index
  col_indx <- rep_len(1:length(ci_column), length(ci_col_list))

  # Draw CI
  for(col_num in seq_along(ci_col_list)){

    if(xlog[col_indx[col_num]]){
      est[[col_num]] <- log(est[[col_num]])
      lower[[col_num]] <- log(lower[[col_num]])
      upper[[col_num]] <- log(upper[[col_num]])
    }

    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next

      if(is_summary[i]){
        draw_ci <- make_summary(est = est[[col_num]][i],
                                lower = lower[[col_num]][i],
                                upper = upper[[col_num]][i],
                                size = sizes[[col_num]][i],
                                xlim = xlim[[col_indx[col_num]]],
                                gp = theme$summary)
      }else {
        draw_ci <- makeci(est = est[[col_num]][i],
                          lower = lower[[col_num]][i],
                          upper = upper[[col_num]][i],
                          size = sizes[[col_num]][i],
                          xlim = xlim[[col_indx[col_num]]],
                          pch = pch_list[col_num],
                          gp = gpar(lty = lty_list[col_num],
                                    lwd = lwd_list[col_num],
                                    col = color_list[col_num]),
                          t_height = theme$ci$t_height,
                          nudge_y = nudge_y[col_num])
      }

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

  # Prepare X axis
  x_axis <- lapply(seq_along(xlim), function(i){
    make_xaxis(at = ticks_at[[i]],
               gp = theme$xaxis,
               x0 = ref_line[i],
               xlim = xlim[[i]],
               xlab = xlab[i],
               is_exp = xlog[i])
  })

  x_axht <- sapply(x_axis, function(x){
    convertHeight(sum(grobHeight(x$children)) + unit(.5, "lines"),
                  unitTo = "mm",
                  valueOnly = TRUE)
  })

  gt <- gtable_add_rows(gt, heights = unit(max(x_axht), "mm"))

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
                          b = tot_row + 1, r = min(ci_column),
                          clip = "off",
                          name = "footnote")
  }

  # Prepare arrow object and row to put it
  if(!is.null(arrow_lab)){
    arrow_grob <- lapply(seq_along(xlim), function(i){
      make_arrow(x0 = ref_line[i],
                 arrow_lab = arrow_lab[[i]],
                 gp = theme$xaxis,
                 is_exp = xlog[i],
                 xlim = xlim[[i]])
    })

    lb_ht <- sapply(arrow_grob, function(x){
      convertHeight(max(grobHeight(x$children)) + unit(.5, "lines"),
                    unitTo = "mm",
                    valueOnly = TRUE)
    })

    gt <- gtable_add_rows(gt, heights = unit(max(lb_ht), "mm"))

  }

  for(j in ci_column){
    idx <- which(ci_column == j)
    # Add reference line
    gt <- gtable_add_grob(gt,
                          vert_line(x = ref_line[idx],
                                    gp = theme$refline,
                                    xlim = xlim[[idx]],
                                    is_exp = xlog[idx]),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          clip = "off",
                          name = paste0("reference.line-", j))

    # Add the X-axis
    gt <- gtable_add_grob(gt, x_axis[[idx]],
                          t = tot_row + 1,
                          l = j,
                          b = tot_row + 1, r = j,
                          clip = "off",
                          name = paste0("xaxis-", j))

    # Add vertical line
    if(!is.null(vert_line))
      gt <- gtable_add_grob(gt,
                            vert_line(x = vert_line[[idx]],
                                      gp = theme$vertline,
                                      xlim = xlim[[idx]],
                                      is_exp = xlog[idx]),
                            t = 2,
                            l = j,
                            b = tot_row, r = j,
                            clip = "off",
                            name = paste0("vert.line-", j))

    # Add arrow
    if(!is.null(arrow_lab))
      gt <- gtable_add_grob(gt, arrow_grob[[idx]],
                            t = nrow(gt), l = j,
                            b = nrow(gt), r = j,
                            clip = "off",
                            name = paste0("arrow-", j))

  }

  # Add legend
  if(group_num > 1){

    by_row <- if(!theme$legend$position %in% c("top", "bottom") || is.null(theme$legend$position)) TRUE else FALSE

    legend <- theme$legend
    legend$pch <- theme$ci$pch
    legend$color <- theme$ci$col
    legend$lty <- theme$ci$lty


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
  gt <- gtable_add_padding(gt, unit(5, "mm"))

  # Auto fit the page
  # gt$widths <- unit(rep(1/ncol(gt), ncol(gt)), "npc")
  # gt$heights <- unit(rep(1/nrow(gt), nrow(gt)), "npc")

  class(gt) <- union("forestplot", class(gt))

  return(gt)

}


#' Draw plot
#'
#' Print or draw forestplot.
#'
#' @param x forestplot to display
#' @param autofit If true, the plot will be autofit.
#' @param ... other arguments not used by this method
#' @return Invisibly returns the original forestplot.
#' @rdname print.forestplot
#' @method print forestplot
#' @export
print.forestplot <- function(x, autofit = FALSE, ...){

  if(autofit){
    # Auto fit the page
    x$widths <- unit(rep(1/ncol(x), ncol(x)), "npc")
    x$heights <- unit(rep(1/nrow(x), nrow(x)), "npc")
  }

  grid.newpage()
  grid.draw(x)

  invisible(x)
}

#' @method plot forestplot
#' @rdname print.forestplot
#' @export
plot.forestplot <- print.forestplot
