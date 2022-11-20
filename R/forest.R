
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
#' If the value is not unique, this will first calculate square root of the
#' reciprocal of size, then devide by overall maximum calculated value.
#' @param ref_line X-axis coordinates of zero line, default is 1. Provide an atomic
#'  vector if different reference line for each \code{ci_column} is desired.
#' @param vert_line Numerical vector, add additional vertical line at given value.
#' Provide a list of numerical vector element if different vertical line for each
#'  \code{ci_column} is desired.
#' @param ci_column Column number of the data the CI will be displayed.
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
#' Although many efforts have been made to automatically get a pretty ticks break,
#' it will not give a perfect solution, especially if \code{'log2'} and \code{'log10'}
#' defined for \code{x_trans}. Please provide this value if possible.
#' @param ticks_digits Number of digits for the x-axis, default is \code{1L}. This
#' should be a numerical vector if different rounding will be applied to different
#' column. If an integer is specified, for example \code{1L}, trailing zeros after
#' the decimal mark will be dropped. Specify numeric, for example \code{1}, to keep
#' the trailing zero after the decimal mark.
#' @param arrow_lab Labels for the arrows, string vector of length two (left and
#' right). The theme of arrow will inherit from the x-axis. This should be a list
#' if different arrow labels for each column is desired.
#' @param x_trans Change axis scale, Allowed values are one of c("none", "log", "log2",
#' "log10"). Default is \code{"none"}, no transformation will be applied.
#' The formatted label will be used for \code{scale  = "log2"} or \code{"log10"}, change
#' this in \code{x_trans}. Set this to \code{"log"} if x-axis tick marks assume values
#'  are exponential, e.g. for logistic regression (OR), survival estimates (HR), Poisson
#'  regression etc.
#' @param xlab X-axis labels, it will be put under the x-axis. An atomic vector should
#' be provided if different \code{xlab} for different column is desired.
#' @param footnote Footnote for the forest plot, will be aligned at left bottom
#' of the plot. Please adjust the line length with line break to avoid the overlap
#' with the arrow and/or x-axis.
#' @param title The text for the title.
#' @param nudge_y Horizontal adjustment to nudge groups by, must be within 0 to 1.
#' @param theme Theme of the forest plot, see \code{\link{forest_theme}} for
#' details.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[gtable]{gtable}} \code{\link[gridExtra]{tableGrob}}
#'  \code{\link{forest_theme}}
#' @example inst/examples/forestplot-example.R
#' @export
#'
#'

forest <- function(data,
                   est,
                   lower,
                   upper,
                   sizes = 0.4,
                   ref_line = ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0),
                   vert_line = NULL,
                   ci_column,
                   is_summary = NULL,
                   xlim = NULL,
                   ticks_at = NULL,
                   ticks_digits = 1L,
                   arrow_lab = NULL,
                   x_trans = "none",
                   xlab = NULL,
                   footnote = NULL,
                   title = NULL,
                   nudge_y = 0,
                   theme = NULL){

  check_errors(data = data, est = est, lower = lower, upper = upper, sizes = sizes,
               ref_line = ref_line, vert_line = vert_line, ci_column = ci_column,
               is_summary = is_summary, xlim = xlim, ticks_at = ticks_at,
               ticks_digits = ticks_digits, arrow_lab = arrow_lab, xlab = xlab,
               title = title, x_trans = x_trans)

  # Set theme
  if(is.null(theme)){
    theme <- forest_theme()
  }

  # For multiple ci_column
  if(length(ref_line) == 1)
    ref_line <- rep(ref_line, length(ci_column))

  if(!is.null(vert_line) && !inherits(vert_line, "list"))
    vert_line <- rep(list(vert_line), length(ci_column))

  if(length(x_trans) == 1)
    x_trans <- rep(x_trans, length(ci_column))

  if(!is.null(xlim) && !inherits(xlim, "list"))
    xlim <- rep(list(xlim), length(ci_column))

  if(!is.null(ticks_at) && !inherits(ticks_at, "list"))
    ticks_at <- rep(list(ticks_at), length(ci_column))

  # ticks digits to accomodate ticks_at
  if(ticks_digits == 1L & !is.null(ticks_at)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        max(nchar(gsub(".*\\.|^[^.]+$", "", as.character(x))))
      })
    else
      ticks_digits <- max(nchar(gsub(".*\\.|^[^.]+$", "", as.character(ticks_digits))))
  }

  if(length(ci_column) != length(ticks_digits))
    ticks_digits <- rep(ticks_digits, length(ci_column))

  if(!is.null(arrow_lab) && !inherits(arrow_lab, "list"))
    arrow_lab <- rep(list(arrow_lab), length(ci_column))

  if(length(xlab) == 1)
    xlab <- rep(xlab, length(ci_column))

    # Replicate sizes
  if(inherits(est, "list") & length(sizes) == 1)
    sizes <- rapply(est, function(x) ifelse(is.na(x), NA, sizes), how = "replace")

  if(is.atomic(est)){
    est <- list(est)
    lower <- list(lower)
    upper <- list(upper)
    if(length(sizes) == 1)
      sizes <- rep(sizes, nrow(data))
    sizes <- list(sizes)
  }

  # Calculate group number
  group_num <- length(est)/length(ci_column)
  ci_col_list <- rep(ci_column, group_num)

  theme <- make_group_theme(theme = theme, group_num = group_num)

  # Get color and pch
  color_list <- rep(theme$ci$col, each = length(ci_column))
  fill_list <- rep(theme$ci$fill, each = length(ci_column))
  alpha_list <- rep(theme$ci$alpha, each = length(ci_column))
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
      rep_tm <- cumsum(c(0, rep(nudge_y, group_num %/% 2)))
      nudge_y <- unique(c(rep_tm, - rep_tm))
    }

    nudge_y <- sort(nudge_y, decreasing = TRUE)

  }

  nudge_y <- rep(nudge_y, each = length(ci_column))


  if(group_num > 1 || is.null(is_summary)){
    if(!is.null(is_summary))
      warning("Summary CI is not supported for multiple groups and will be ignored.")
    is_summary <- rep(FALSE, nrow(data))
  }

  if(length(unique(stats::na.omit(unlist(sizes)))) != 1){
    # Get the maximum reciprocal of size
    max_sizes <- sapply(sizes, function(x){
      x <- sqrt(1/x)
      max(x[!is_summary], na.rm = TRUE)
    }, USE.NAMES = FALSE)

    sizes <- lapply(sizes, function(x){
      wi <- sqrt(1/x)
      wi <- wi/max(max_sizes, na.rm = TRUE)
      wi[is_summary] <- 1/length(max_sizes)
      return(wi)
    })
  }

  # Positions of values in ci_column
  gp_list <- rep_len(1:(length(lower)/group_num), length(lower))

  # Check exponential
  if(any(x_trans %in% c("log", "log2", "log10"))){
    for(i in seq_along(x_trans)){
      if(x_trans[i] %in% c("log", "log2", "log10")){
        sel_num <- gp_list == i
        checks_ill <- c(any(unlist(est[sel_num]) <= 0, na.rm = TRUE),
              any(unlist(lower[sel_num]) <= 0, na.rm = TRUE),
              any(unlist(upper[sel_num]) <= 0, na.rm = TRUE),
              (any(ref_line[i] <= 0)),
              (!is.null(vert_line) && any(unlist(vert_line[[i]]) <= 0, na.rm = TRUE)),
              (!is.null(xlim) && any(unlist(xlim[[i]]) < 0)))
        zeros <- c("est", "lower", "upper", "ref_line", "vert_line", "xlim")
        if (any(checks_ill)) {
          message("found values equal or less than 0 in ", zeros[checks_ill])
          stop("est, lower, upper, ref_line, vert_line and xlim should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
        }
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
              x_trans = x_trans[i])
  })

  # Set X-axis breaks if missing
  ticks_at <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_at[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               x_trans = x_trans[i])
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

    # Get current CI column and group number
    current_col <- ci_col_list[col_num]
    current_gp <- sum(col_indx[1:col_num] == col_indx[col_num])

    # Convert value is exponentiated
    col_trans <- x_trans[col_indx[col_num]]
    if(col_trans != "none"){
      est[[col_num]] <- xscale(est[[col_num]], col_trans)
      lower[[col_num]] <- xscale(lower[[col_num]], col_trans)
      upper[[col_num]] <- xscale(upper[[col_num]], col_trans)
    }

    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next

      if(is.na(lower[[col_num]][i]) || is.na(upper[[col_num]][i])){
        warning("Missing lower and/or upper limit on column", current_col, " row ", i)
        next
      }

      if(is_summary[i]){
        draw_ci <- make_summary(est = est[[col_num]][i],
                                lower = lower[[col_num]][i],
                                upper = upper[[col_num]][i],
                                sizes = sizes[[col_num]][i],
                                xlim = xlim[[col_indx[col_num]]],
                                gp = theme$summary)
      }else {
        draw_ci <- makeci(est = est[[col_num]][i],
                          lower = lower[[col_num]][i],
                          upper = upper[[col_num]][i],
                          sizes = sizes[[col_num]][i],
                          xlim = xlim[[col_indx[col_num]]],
                          pch = pch_list[col_num],
                          gp = gpar(lty = lty_list[col_num],
                                    lwd = lwd_list[col_num],
                                    col = color_list[col_num],
                                    fill = fill_list[col_num],
                                    alpha = alpha_list[col_num]),
                          t_height = theme$ci$t_height,
                          nudge_y = nudge_y[col_num])
      }

      # Skip if CI is outside xlim
      if(upper[[col_num]][i] < min(xlim[[col_indx[col_num]]]) | lower[[col_num]][i] > max(xlim[[col_indx[col_num]]])){
        message("The confidence interval of row ", i, ", column ", current_col, ", group ", current_gp,
                " is outside of the xlim.")
        next
      }


      gt <- gtable_add_grob(gt, draw_ci,
                            t = i + 1,
                            l = current_col,
                            b = i + 1,
                            r = current_col,
                            clip = "off",
                            name = paste0("ci-", i, "-", current_col, "-", current_gp))
    }
  }

  tot_row <- nrow(gt)

  # Prepare X axis
  x_axis <- lapply(seq_along(xlim), function(i){
    make_xaxis(at = ticks_at[[i]],
               gp = theme$xaxis,
               ticks_digits = ticks_digits[i],
               x0 = ref_line[i],
               xlim = xlim[[i]],
               xlab = xlab[i],
               x_trans = x_trans[i])
  })

  x_axht <- sapply(x_axis, function(x){
    ht <- Reduce(`+`, lapply(x$children, grobHeight))
    convertHeight(ht, unitTo = "mm", valueOnly = TRUE)
  })

  gt <- gtable_add_rows(gt, heights = unit(max(x_axht), "mm") + unit(.8, "lines"))

  # Prepare arrow object and row to put it
  if(!is.null(arrow_lab)){
    arrow_grob <- lapply(seq_along(xlim), function(i){
      make_arrow(x0 = ref_line[i],
                 arrow_lab = arrow_lab[[i]],
                 arrow_gp = theme$arrow,
                 x_trans = x_trans[i],
                 col_width = convertWidth(gt$widths[ci_column[i]], "char", valueOnly = TRUE),
                 xlim = xlim[[i]])
    })

    lb_ht <- sapply(arrow_grob, function(x){
      ht <- Reduce(`+`, lapply(x$children, heightDetails))
      convertHeight(ht, unitTo = "mm", valueOnly = TRUE)
    })

    gt <- gtable_add_rows(gt, heights = unit(max(lb_ht), "mm"))

  }

  # Add footnote
  if(!is.null(footnote)){
    footnote_grob <- textGrob(label = footnote,
                              gp = theme$footnote,
                              x = 0,
                              y = .8,
                              just = "left",
                              check.overlap = TRUE,
                              name = "footnote")

    gt <- gtable_add_grob(gt,
                          footnote_grob,
                          t = tot_row + 1,
                          l = 1,
                          b = nrow(gt), r = min(ci_column),
                          clip = "off",
                          name = "footnote")
  }

  for(j in ci_column){
    idx <- which(ci_column == j)
    # Add reference line
    gt <- gtable_add_grob(gt,
                          vert_line(x = ref_line[idx],
                                    gp = theme$refline,
                                    xlim = xlim[[idx]],
                                    x_trans = x_trans[idx]),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          clip = "off",
                          name = paste0("ref.line-", j))

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
                                      x_trans = x_trans[idx]),
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
  if(group_num > 1 & theme$legend$position != "none"){

    by_row <- !theme$legend$position %in% c("top", "bottom")

    legend <- theme$legend
    legend$pch <- theme$ci$pch
    legend$gp$col <- theme$ci$col
    legend$gp$lty <- theme$ci$lty
    legend$gp$fill <- theme$ci$fill


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

  if(!is.null(title)){
    max_height <- max(convertHeight(stringHeight(title), "mm", valueOnly = TRUE))
    gt <- gtable_add_rows(gt, unit(max_height, "mm") + unit(2, "mm"), pos = 0)
    title_x <- switch(theme$title$just,
                      right = unit(1, "npc"),
                      left  = unit(0, "npc"),
                      center = unit(.5, "npc"))
    title_gb <- textGrob(label = title,
                         gp = theme$title$gp,
                         x = title_x,
                         just = theme$title$just,
                         check.overlap = TRUE,
                         name = "plot.title")

    gt <- gtable_add_grob(gt, title_gb,
                          t = 1,
                          b = 1,
                          l = 1,
                          r = ncol(gt),
                          clip = "off",
                          name = "plot.title")
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
