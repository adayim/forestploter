
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
#' Values will be used as it is, no transformation will be applied.
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
#' @param ticks_digits Number of digits for the x-axis, default is \code{NULL} to calculate
#' an integer based on \code{ticks_at} if provided or \code{lower} and \code{upper} if not.
#'  This should be a numerical vector if different rounding will be applied to different
#'  column. If an integer is specified, for example \code{1L}, trailing zeros after
#' the decimal mark will be dropped. Specify numeric, for example  \code{1}, to keep
#'  the trailing zero after the decimal mark.
#' @param ticks_minor A numeric vector of positions to draw ticks without labels (optional).
#' The values provided here can be superset or disjoint sets of \code{ticks_at}.
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
#' @param fn_ci Name of the function to draw confidence interval, default is
#' \code{\link{makeci}}. You can specify your own drawing function to draw the
#' confidence interval, but the function needs to accept arguments \code{
#' "est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y"}.
#' Please refer to the \code{\link{makeci}} function for the details of these
#' parameters.
#' @param fn_summary Name of the function to draw summary confidence interval,
#' default is \code{\link{make_summary}}. You can specify your own drawing
#' function to draw the summary confidence interval, but the function needs to
#'  accept arguments \code{"est", "lower", "upper", "sizes", "xlim", "gp"}.
#' Please refer to the \code{\link{make_summary}} function for the details of
#'  these parameters.
#' @param index_args A character vector, name of the arguments used for indexing
#'  the row and column. This should be the name of the arguments that is working
#' the same way as \code{est}, \code{lower} and \code{upper}. Check out the
#' examples in the \code{\link{make_boxplot}}.
#' @param theme Theme of the forest plot, see \code{\link{forest_theme}} for
#' details.
#' @param ... Other arguments passed on to the \code{fn_ci} and \code{fn_summary}.
#'
#' @importFrom stats na.omit
#'
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[gtable]{gtable}} \code{\link[gridExtra]{tableGrob}}
#'  \code{\link{forest_theme}} \code{\link{make_boxplot}}
#' \code{\link{makeci}}  \code{\link{make_summary}}
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
                   ticks_digits = NULL,
                   ticks_minor = NULL,
                   arrow_lab = NULL,
                   x_trans = "none",
                   xlab = NULL,
                   footnote = NULL,
                   title = NULL,
                   nudge_y = 0,
                   fn_ci = makeci,
                   fn_summary = make_summary,
                   index_args = NULL,
                   theme = NULL,
                   ...){

  dot_args <- list(...)

  # Check arguments
  args_ci <- names(formals(fn_ci))
  if(!all(c("est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y") %in% args_ci))
    stop("arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"pch\", \"gp\", \"t_height\" and \"nudge_y\" must be provided in the function `fn_ci`.")

  args_summary <- names(formals(fn_summary))
  if(any(unlist(is_summary))){
    if(!all(c("est", "lower", "upper", "sizes", "xlim", "gp", "nudge_y") %in% args_summary))
    stop("arguments \"est\", \"lower\", \"upper\", \"sizes\", \"nudge_y\", \"xlim\",and \"gp\" must be provided in the function `fn_summary`.")
  }

  check_errors(data = data, est = est, lower = lower, upper = upper, sizes = sizes,
               ref_line = ref_line, vert_line = vert_line, ci_column = ci_column,
               is_summary = is_summary, xlim = xlim, ticks_at = ticks_at,
               ticks_digits = ticks_digits, arrow_lab = arrow_lab, xlab = xlab,
               title = title, x_trans = x_trans, ticks_minor = ticks_minor)

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

  if(is.null(ticks_at)){
      ticks_at <- ticks_minor
  }else{
    if(!inherits(ticks_at, "list"))
      ticks_at <- rep(list(ticks_at), length(ci_column))
  }

  if(is.null(ticks_minor)){
    ticks_minor <- ticks_at
  }else{
    if(!inherits(ticks_minor, "list"))
      ticks_minor <- rep(list(ticks_minor), length(ci_column))
  }

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

  # Check index_var
  if(!is.null(index_args)){
    for(ind_v in index_args){
      if(!is.list(dot_args[[ind_v]]))
        dot_args[[ind_v]] <- list(dot_args[[ind_v]])

      est_len <- vapply(est, length, FUN.VALUE = 1L)
      arg_len <- vapply(dot_args[[ind_v]], length, FUN.VALUE = 1L)
      if(length(dot_args[[ind_v]]) != length(est) || length(unique(c(est_len, arg_len))) != 1)
        stop("index_args should have the same length as est.")
    }
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

  # Positions of values in ci_column
  gp_list <- rep_len(1:(length(lower)/group_num), length(lower))

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


  if(is.null(is_summary)){
    is_summary <- rep(FALSE, nrow(data))
  }

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

  # automatic calculation of tick digits if missing
  if(is.null(ticks_digits) & !is.null(ticks_at)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        max(count_decimal(x))
      })
    else
      ticks_digits <- max(count_decimal(ticks_digits))

    ticks_digits <- as.integer(ticks_digits)
  }

  # Set xlim to minimum and maximum value of the CI
  xlim <- lapply(seq_along(ci_column), function(i){
    sel_num <- gp_list == i
    make_xlim(xlim = xlim[[i]],
              lower = lower[sel_num],
              upper = upper[sel_num],
              ref_line = ref_line[i],
              ticks_at = c(ticks_at[[i]], ticks_minor[[i]]),
              x_trans = x_trans[i])
  })

  # Set X-axis breaks if missing
  ticks_at <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_at[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               x_trans = x_trans[i])
  })

  ticks_minor <- lapply(seq_along(xlim), function(i){
    make_ticks(at = ticks_minor[[i]],
               xlim = xlim[[i]],
               refline = ref_line[i],
               x_trans = x_trans[i])
  })

  # ticks digits auto calculation if missing
  if(is.null(ticks_digits)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        count_zeros(x)
      }, USE.NAMES = FALSE)
    else{
      ticks_digits <- count_zeros(ticks_at)
    }
    ticks_digits <- as.integer(ticks_digits)
  }

  if(length(ci_column) != length(ticks_digits))
    ticks_digits <- rep(ticks_digits, length(ci_column))

  gt <- tableGrob(data, theme = theme$tab_theme, rows = NULL)

  if(group_num > 1 & any(is_summary)){
    gt$heights[c(FALSE, is_summary)] <- gt$heights[c(FALSE, is_summary)]*2
  }


  # Do not clip text
  gt$layout$clip <- "off"

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

      # Transform other indexing arguments
      if(!is.null(index_args)){
        for(ind_v in index_args){
          if(any(unlist(dot_args[[ind_v]][[col_num]]) <= 0, na.rm = TRUE) & col_trans %in% c("log", "log2", "log10"))
            stop(ind_v, " should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
          dot_args[[ind_v]][[col_num]] <- xscale(dot_args[[ind_v]][[col_num]], col_trans)
        }
      }
    }

    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next

      if(is.na(lower[[col_num]][i]) || is.na(upper[[col_num]][i])){
        warning("Missing lower and/or upper limit on column", current_col, " row ", i)
        next
      }

      dot_pass <- dot_args
      if(!is.null(index_args)){
        for(ind_v in index_args){
          dot_pass[[ind_v]] <- dot_pass[[ind_v]][[col_num]][i]
        }
      }

      if(is_summary[i]){
        # Update graphical parameters
        g_par <- gpar(col = theme$summary$col[current_gp],
                      fill = theme$summary$fill[current_gp])
        if ("gp" %in% names(dot_pass)) {
          g_par <- modifyList(list(dot_pass$gp), g_par)
          dot_pass$gp <- NULL
        }

        dot_pass <- dot_pass[names(dot_pass) %in% args_summary]

        draw_ci <- do.call(fn_summary, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = xlim[[col_indx[col_num]]],
               gp = g_par,
               nudge_y = nudge_y[col_num]),
          dot_pass
        ))

      }else {
        # Update graphical parameters
        g_par <- gpar(lty = lty_list[col_num],
                      lwd = lwd_list[col_num],
                      col = color_list[col_num],
                      fill = fill_list[col_num],
                      alpha = alpha_list[col_num])

        if ("gp" %in% names(dot_pass)) {
          g_par <- modifyList(dot_pass$gp, g_par)
          dot_pass$gp <- NULL
        }

        dot_pass <- dot_pass[names(dot_pass) %in% args_ci]

        draw_ci <- do.call(fn_ci, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = xlim[[col_indx[col_num]]],
               pch = pch_list[col_num],
               gp = g_par,
               t_height = theme$ci$t_height,
               nudge_y = nudge_y[col_num]),
          dot_pass
        ))
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
               at_minor = ticks_minor[[i]],
               gp = theme$xaxis,
               xlab_gp = theme$xlab,
               ticks_digits = ticks_digits[[i]],
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
    if(theme$footnote$parse)
      footnote <- tryCatch(parse(text = footnote), error = function(e) footnote)
    footnote_grob <- textGrob(label = footnote,
                              gp = theme$footnote$gp,
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
                                    x_trans = x_trans[idx],
                                    nrow = nrow(data)),
                          t = 2,
                          l = j,
                          b = tot_row, r = j,
                          # Make sure reference line is below the whisker
                          z = max(gt$layout$z[grepl("core-", gt$layout$name)]),
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
                                      x_trans = x_trans[idx],
                                      nrow = nrow(data)),
                            t = 2,
                            l = j,
                            b = tot_row, r = j,
                            z = max(gt$layout$z[grepl("core-", gt$layout$name)]),
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
