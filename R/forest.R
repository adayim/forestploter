
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
#' @param ref_line X-axis coordinates of zero line, default is 1.
#' @param vert_line Numerical vector, add additional vertical line at given value.
#' @param ci_column Column number of the data the CI will be displayed.
#' @param xlog If TRUE, x-axis tick marks assume values are exponential, e.g.
#' for logistic regression (OR), survival estimates (HR), Poisson regression etc.
#' @param is_summary A logical vector indicating if the value is a summary value,
#' which will have a diamond shape for the estimate. Can not be used with multiple
#' group `forestplot`.
#' @param xlim Limits for the x axis as a vector of length 2, i.e. c(low, high). It
#' will take the minimum and maximum of the lower and upper value if not provided.
#' This will apply to all CI columns if provided, and will be calculated automatically
#' each column if not provided.
#' @param ticks_at Set X-axis tick-marks point. This will apply to all CI columns if
#' provided, and will be calculated automatically for each column if not provided.
#' @param arrow_lab Labels for the arrows, string vector of length two (left and
#' right). The theme of arrow will inherit from the x-axis.
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
                   footnote = NULL,
                   nudge_y = 0,
                   theme = NULL){

  # Point sizes
  if(length(sizes) == 1 & !inherits(sizes, "list"))
    sizes <- rep(sizes, nrow(data))

  # Check arrow
  if(!is.null(arrow_lab) & length(arrow_lab) != 2)
    stop("Arrow label must of length 2.")

  # Set theme
  if(is.null(theme)){
    theme <- forest_theme()
  }

  # Check xlim
  if(inherits(xlim, "list"))
    stop("xlim can not be list.")

  # Check vertical line
  if(!is.null(vert_line) && !is.numeric(vert_line))
    stop("vert_line must be a numeric vector.")

  # Default color set
  col_set <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
               "#ffff33","#a65628","#f781bf","#999999")

  # Check length
  if(length(unique(c(length(est), length(lower), length(upper)))) != 1)
      stop("Estimate, lower and upper should have the same length.")

  # Check length for the summary
  if(!is.null(is_summary) && length(is_summary) != nrow(data))
    stop("is_summary should have same legnth as data rownumber.")

  # Check the break
  if(!is.null(ticks_at) && !is.null(xlim)){
    if(max(ticks_at) > max(xlim) || min(ticks_at) < min(xlim))
      warning("ticks_at is outside the xlim.")
  }

  # Chekc exponential
  if(xlog){
    if (any(unlist(est) < 0, na.rm = TRUE) ||
        any(unlist(lower) < 0, na.rm = TRUE) ||
        any(unlist(upper) < 0, na.rm = TRUE) ||
        (!is.na(ref_line) && ref_line <= 0) ||
        (!missing(vert_line) && any(vert_line <= 0, na.rm = TRUE)) ||
        (!missing(xlim) && xlim[1] < 0)) {
      stop("est, lower, upper, ref_line, vert_line and xlim should be provided in exponential form if `xlog=TRUE`.")
    }

    ref_line <- log(ref_line)

    if(!is.null(vert_line))
      vert_line <- log(vert_line)

  }

  if(inherits(est, "list") | inherits(lower, "list") | inherits(upper, "list")){

    if(!inherits(est, "list") | !inherits(lower, "list") | !inherits(upper, "list"))
      stop("Estimate, lower and upper must be a list if plan to plot CI for multiple columns.")

    # Calculate group number
    group_num <- length(est)/length(ci_column)
    ci_col_list <- rep(ci_column, group_num)

    # Replicate sizes to align with est and CIs
    if(!inherits(sizes, "list"))
      sizes <- rep(list(sizes), length(ci_col_list))

    # If color is given and not have the same length as group number
    if(group_num > 1 & length(theme$ci$col) == 1)
      theme$ci$col <- col_set[1:group_num]

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

    # Check for group and color
    if(group_num > 1 & length(theme$ci$col) < group_num & length(theme$ci$col) > 1)
      stop("More groups than colors.")

    # Check for group and legend label
    if(group_num > 1 & length(theme$legend$label) < group_num & length(theme$legend$label) > 1)
      stop("More groups than legend labels.")

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

  # Set xlim to minimum and maximum value of the CI
  xlim <- make_xlim(xlim = xlim,
                    lower = lower,
                    upper = upper,
                    gp_num = group_num,
                    ref_line = ref_line,
                    ticks_at = ticks_at,
                    is_exp = xlog)


  # Set X-axis breaks if missing
  ticks_at <- lapply(xlim, function(xlm){
    make_ticks(at = ticks_at, xlim = xlm, refline = ref_line, is_exp = xlog)
  })


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

  # Column index
  col_indx <- rep_len(1:group_num, length(ci_col_list))

  # Draw CI
  for(col_num in seq_along(ci_col_list)){

    if(xlog){
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
               xlim = xlim[[i]],
               is_exp = xlog)
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
      make_arrow(x0 = ref_line,
                 arrow_lab = arrow_lab,
                 gp = theme$xaxis,
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
                          vert_line(x = ref_line, gp = theme$refline,
                                    xlim = xlim[[idx]]),
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
                            vert_line(x = vert_line, gp = theme$vertline,
                                      xlim = xlim[[idx]]),
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
