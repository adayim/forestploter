
# Draw the gtable described by a recipe, see `forest()`.
#
# Values that can differ between CI columns (`xlim`, `ticks_at`,
# `ticks_minor`, `ticks_digits`, `vert_line`, `arrow_lab` and `xlab`) are lists
# with one element per CI column, `NULL` meaning not set, and `x_trans` is a
# vector of the same length.
#
# The order in which grobs are added to the gtable must stay as it is: it sets
# the drawing order, and with it the visual snapshots. The parts are added by
# `add_ci_grobs()`, `add_axis_grobs()`, `add_legend_grob()` and
# `add_title_grob()` below, in that order.
draw_forest <- function(recipe){

  data <- recipe$data
  est <- recipe$est
  lower <- recipe$lower
  upper <- recipe$upper
  sizes <- recipe$sizes
  ci_column <- recipe$ci_column
  is_summary <- recipe$is_summary
  nudge_y <- recipe$ci$nudge_y
  fn_ci <- recipe$ci$fn_ci
  fn_summary <- recipe$ci$fn_summary
  index_args <- recipe$ci$index_args
  dot_args <- recipe$ci$dots
  xlim <- recipe$xaxis$xlim
  ticks_at <- recipe$xaxis$ticks_at
  ticks_digits <- recipe$xaxis$ticks_digits
  ticks_minor <- recipe$xaxis$ticks_minor
  x_trans <- recipe$xaxis$x_trans
  vert_line <- recipe$xaxis$vline
  arrow_lab <- recipe$labs$arrow
  xlab <- recipe$labs$xlab
  footnote <- recipe$labs$footnote
  title <- recipe$labs$title

  theme <- resolve_theme(recipe)

  # Parse the labels as plotmath if the style asks for it
  if(isTRUE(theme$parse_text)){
    title <- parse_label(title)
    xlab <- lapply(xlab, parse_label)
    arrow_lab <- lapply(arrow_lab, parse_label)
    theme$legend$label <- parse_label(theme$legend$label)
  }

  # The default reference line follows the axis scale
  ref_line <- recipe$ref_line
  if(is.null(ref_line))
    ref_line <- default_ref_line(x_trans)

  args_ci <- names(formals(fn_ci))
  args_summary <- names(formals(fn_summary))

  # For multiple ci_column
  if(length(ref_line) == 1)
    ref_line <- rep(ref_line, length(ci_column))

  # Tick positions and minor ticks fall back on each other
  for(i in seq_along(ci_column)){
    if(is.null(ticks_at[[i]]))
      ticks_at[i] <- list(ticks_minor[[i]])

    if(is.null(ticks_minor[[i]]))
      ticks_minor[i] <- list(ticks_at[[i]])
  }

  has_arrow <- vapply(seq_along(ci_column), function(i){
    !is.null(arrow_lab[[i]])
  }, FUN.VALUE = logical(1))

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

  sizes <- scale_weights(sizes, is_summary, data, recipe$size_scale, recipe)
  size_flat <- unlist(sizes)

  dot_args <- check_index_args(dot_args, index_args, est)

  # Calculate group number
  group_num <- length(est)/length(ci_column)
  ci_col_list <- rep(ci_column, group_num)

  if(!is.null(recipe$legend$labels) && length(recipe$legend$labels) != group_num)
    stop("legend_labels should be provided for each group.")

  theme <- make_group_theme(theme = theme, group_num = group_num)

  ci_gp <- group_gpars(theme, ci_column)

  # Positions of values in ci_column
  gp_list <- rep_len(1:(length(lower)/group_num), length(lower))

  nudge_y <- group_nudge(nudge_y, group_num, ci_column)

  warn_overlap(nudge_y, size_flat, theme, group_num, recipe)

  if(is.null(is_summary)){
    is_summary <- rep(FALSE, nrow(data))
  }

  check_log_values(est, lower, upper, ref_line, vert_line, xlim, x_trans, gp_list)

  axis <- axis_values(xlim, ticks_at, ticks_minor, ticks_digits, x_trans,
                      ref_line, lower, upper, gp_list, ci_column)
  xlim <- axis$xlim
  ticks_at <- axis$ticks_at
  ticks_minor <- axis$ticks_minor
  ticks_digits <- axis$ticks_digits

  gt <- cached_table(data, theme$tab_theme)

  gt <- summary_row_heights(gt, is_summary, nudge_y, size_flat, theme, group_num)

  # Do not clip text
  gt$layout$clip <- "off"

  # Column index
  col_indx <- rep_len(1:length(ci_column), length(ci_col_list))

  gt <- add_ci_grobs(gt, data, est, lower, upper, sizes, is_summary, xlim, x_trans,
                     ci_col_list, col_indx, nudge_y, ci_gp, theme, fn_ci, fn_summary,
                     args_ci, args_summary, dot_args, index_args)

  gt <- add_axis_grobs(gt, data, ci_column, xlim, ticks_at, ticks_minor, ticks_digits,
                       x_trans, ref_line, xlab, arrow_lab, has_arrow, vert_line,
                       footnote, theme)

  gt <- add_legend_grob(gt, theme, group_num)

  gt <- add_title_grob(gt, title, theme)

  # Add padding
  gt <- gtable_add_padding(gt, unit(5, "mm"))

  class(gt) <- union("forestplot", class(gt))

  return(gt)

}

# Read `sizes` as weights and scale them if `scale_sizes()` asked for it.
# Normalised jointly across all groups and CI columns so the areas stay
# comparable between them; scale by hand if per-column control is wanted.
#
# Summary rows are held out. Their weight is a pooled total rather than a
# study weight, so leaving them in would drag the whole normalisation towards
# the summary and squash the studies into a narrow band. They are then drawn
# at the top of the range, which is what `meta` does: its pooled rows have no
# study weight, so they fall to the fixed `1 * squaresize`, the same size as
# the largest study square. `metafor` likewise sizes its summary polygon from
# `efac` alone and never from the weights.
#
# Sizes that will not render sensibly are warned about here rather than in
# `check_errors`, so that the values checked are the ones actually drawn, not
# the raw weights. The warning is given when the plot is drawn, as
# `scale_sizes()` may still follow in a pipe.
scale_weights <- function(sizes, is_summary, data, size_scale, recipe){

  if(!is.null(size_scale$method)){
    summ_row <- if(is.null(is_summary)) rep(FALSE, nrow(data)) else is_summary

    if(all(lengths(sizes) == length(summ_row)))
      summ_flat <- rep(summ_row, length(sizes))
    else
      summ_flat <- rep(FALSE, length(unlist(sizes)))

    size_flat <- unlist(sizes)
    keep <- !is.na(size_flat)
    size_flat[summ_flat] <- NA

    size_flat <- weights_to_sizes(size_flat, range = size_scale$range,
                                  method = size_scale$method)
    size_flat[summ_flat & keep] <- size_scale$range[2]

    sizes <- relist(size_flat, sizes)
  }

  size_flat <- unlist(sizes)
  if(any(size_flat < 0.1, na.rm = TRUE) || any(size_flat > 2, na.rm = TRUE))
    defer_warning(recipe,
                  "`sizes` outside the usual range of 0.1 to 2: values below 0.1 draw ",
                  "smaller than a point and are invisible, values above 2 are taller ",
                  "than the row and overlap neighbouring rows. Weight-based sizes ",
                  "typically fall between 0.2 and 1.5; see `scale_sizes()`.")

  sizes
}

# Check index_var, the values of `index_args` are given for each element of
# `est` and kept as lists
check_index_args <- function(dot_args, index_args, est){

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

  dot_args
}

# Get color and pch of the confidence intervals, one value for each element of
# `est`
group_gpars <- function(theme, ci_column){
  list(col = rep(theme$ci$col, each = length(ci_column)),
       fill = rep(theme$ci$fill, each = length(ci_column)),
       alpha = rep(theme$ci$alpha, each = length(ci_column)),
       pch = rep(theme$ci$pch, each = length(ci_column)),
       lty = rep(theme$ci$lty, each = length(ci_column)),
       lwd = rep(theme$ci$lwd, each = length(ci_column)))
}

# Vertical offset of each group, one value for each element of `est`
group_nudge <- function(nudge_y, group_num, ci_column){

  # Check nudge_y
  if(nudge_y >= 1 || nudge_y < 0)
    stop("`nudge_y` must be within 0 to 1.")

  if(group_num > 1 && nudge_y == 0)
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

  rep(nudge_y, each = length(ci_column))
}

# Grouped CIs share one cell, so a point taller than the gap between two group
# offsets overlaps its neighbour. The row height is only known once the plot is
# drawn, so estimate it from the theme rather than measuring the device: a row
# is the text height plus the vertical cell padding, which tracks
# `0.72 * base_size + padding` closely across base sizes.
#
# Only a point more than twice the gap is reported, i.e. one overlapping its
# neighbour by at least half its own height. Points touching slightly are
# common and legible, and the estimate runs low for rows holding more than one
# line of text, so a tighter bound would cry wolf.
warn_overlap <- function(nudge_y, size_flat, theme, group_num, recipe){

  if(group_num > 1){
    gap_npc <- min(diff(sort(unique(nudge_y))))
    row_pt <- 0.72 * theme$base_size + core_padding_bigpts(theme)
    max_size <- max(size_flat, na.rm = TRUE)
    if(max_size * theme$base_size > 2 * gap_npc * row_pt)
      defer_warning(recipe,
                    "Grouped confidence intervals are likely to overlap: a point of ",
                    "`sizes` ", signif(max_size, 3), " is more than twice the gap ",
                    "between groups. Increase `nudge_y` or reduce `sizes`.")
  }

  invisible()
}

# Check exponential
check_log_values <- function(est, lower, upper, ref_line, vert_line, xlim, x_trans, gp_list){

  if(any(x_trans %in% c("log", "log2", "log10"))){
    for(i in seq_along(x_trans)){
      if(x_trans[i] %in% c("log", "log2", "log10")){
        sel_num <- gp_list == i
        checks_ill <- c(any(unlist(est[sel_num]) <= 0, na.rm = TRUE),
              any(unlist(lower[sel_num]) <= 0, na.rm = TRUE),
              any(unlist(upper[sel_num]) <= 0, na.rm = TRUE),
              (any(ref_line[i] <= 0)),
              (any(unlist(vert_line[[i]]) <= 0, na.rm = TRUE)),
              (any(unlist(xlim[[i]]) < 0)))
        zeros <- c("est", "lower", "upper", "ref_line", "vline", "xlim")
        if (any(checks_ill)) {
          message("found values equal or less than 0 in ", zeros[checks_ill])
          stop("est, lower, upper, ref_line, vline and xlim should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
        }
      }
    }
  }

  invisible()
}

# Limits, tick positions and tick digits of each CI column, filling in the
# ones that are not set
axis_values <- function(xlim, ticks_at, ticks_minor, ticks_digits, x_trans,
                        ref_line, lower, upper, gp_list, ci_column){

  # automatic calculation of tick digits if missing
  for(i in seq_along(ci_column)){
    if(is.null(ticks_digits[[i]]) && !is.null(ticks_at[[i]]))
      ticks_digits[[i]] <- as.integer(max(count_decimal(ticks_at[[i]])))
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
  for(i in seq_along(ci_column)){
    if(is.null(ticks_digits[[i]]))
      ticks_digits[[i]] <- as.integer(count_zeros(ticks_at[[i]]))
  }

  list(xlim = xlim,
       ticks_at = ticks_at,
       ticks_minor = ticks_minor,
       ticks_digits = ticks_digits)
}

# Stacked group diamonds need room. A diamond of height `s` big points sitting
# at npc offset `o` fits only if the row is at least `(s/2) / (0.5 - |o|)`
# tall, so grow the summary rows to what the offsets actually need instead of
# a flat doubling. `unit.pmax` resolves lazily at draw time, so the natural
# row height never has to be measured here.
summary_row_heights <- function(gt, is_summary, nudge_y, size_flat, theme, group_num){

  if(group_num > 1 && any(is_summary)){
    off <- abs(nudge_y)
    off <- off[off < 0.5]
    if(length(off) > 0){
      half_pt <- max(size_flat, na.rm = TRUE) * theme$base_size / 2
      h_req <- max(half_pt / (0.5 - off))
      sum_rows <- c(FALSE, is_summary)
      gt$heights[sum_rows] <- unit.pmax(gt$heights[sum_rows],
                                        unit(h_req, "bigpts"))
    }
  }

  gt
}

# Draw CI, one batch of grobs for each CI column and group
add_ci_grobs <- function(gt, data, est, lower, upper, sizes, is_summary, xlim, x_trans,
                         ci_col_list, col_indx, nudge_y, ci_gp, theme, fn_ci, fn_summary,
                         args_ci, args_summary, dot_args, index_args){

  for(col_num in seq_along(ci_col_list)){

    # Get current CI column and group number
    current_col <- ci_col_list[col_num]
    current_gp <- sum(col_indx[1:col_num] == col_indx[col_num])
    col_xlim <- xlim[[col_indx[col_num]]]

    # Convert value is exponentiated
    col_trans <- x_trans[col_indx[col_num]]
    if(col_trans != "none"){
      est[[col_num]] <- xscale(est[[col_num]], col_trans)
      lower[[col_num]] <- xscale(lower[[col_num]], col_trans)
      upper[[col_num]] <- xscale(upper[[col_num]], col_trans)

      # Transform other indexing arguments
      if(!is.null(index_args)){
        for(ind_v in index_args){
          if(any(unlist(dot_args[[ind_v]][[col_num]]) <= 0, na.rm = TRUE) && col_trans %in% c("log", "log2", "log10"))
            stop(ind_v, " should be larger than 0, if `x_trans` in \"log\", \"log2\", \"log10\".")
          dot_args[[ind_v]][[col_num]] <- xscale(dot_args[[ind_v]][[col_num]], col_trans)
        }
      }
    }

    # ---- Hoist per-column invariants out of the row loop ----
    # User-supplied gp from `...` (consumed once; per-row dot_pass$gp <- NULL
    # in the original was redundant against a fresh per-row copy).
    user_gp <- dot_args[["gp"]]

    # `fontsize` is what makes `sizes` mean "a multiple of one line of text";
    # see `size_bigpts()`.
    ci_gpar <- gpar(lty = ci_gp$lty[col_num],
                    lwd = ci_gp$lwd[col_num],
                    col = ci_gp$col[col_num],
                    fill = ci_gp$fill[col_num],
                    alpha = ci_gp$alpha[col_num],
                    fontsize = theme$base_size)
    if(!is.null(user_gp))
      ci_gpar <- modifyList(user_gp, ci_gpar)

    summary_gp <- gpar(col = theme$summary$col[current_gp],
                       fill = theme$summary$fill[current_gp],
                       fontsize = theme$base_size)
    if(!is.null(user_gp))
      summary_gp <- modifyList(user_gp, summary_gp)

    # Static (non-row-dependent) extras for fn_ci / fn_summary. `gp` and the
    # per-row `index_args` are added inside the loop below.
    static_dot_args <- dot_args
    static_dot_args[["gp"]] <- NULL
    if(!is.null(index_args))
      static_dot_args[index_args] <- NULL
    ci_extra      <- static_dot_args[names(static_dot_args) %in% args_ci]
    summary_extra <- static_dot_args[names(static_dot_args) %in% args_summary]

    col_min <- min(col_xlim)
    col_max <- max(col_xlim)

    # Collect grobs and their row positions, then add to gt in one batch
    grob_list <- vector("list", nrow(data))
    t_pos <- integer(nrow(data))
    name_list <- character(nrow(data))
    n_grobs <- 0L

    for(i in 1:nrow(data)){
      if(is.na(est[[col_num]][i]))
        next

      if(is.na(lower[[col_num]][i]) || is.na(upper[[col_num]][i])){
        warning("Missing lower and/or upper limit on column ", current_col, " row ", i)
        next
      }

      # Skip if CI is outside xlim before building any grob
      if(upper[[col_num]][i] < col_min || lower[[col_num]][i] > col_max){
        message("The confidence interval of row ", i, ", column ", current_col, ", group ", current_gp,
                " is outside of the xlim.")
        next
      }

      # Per-row index_args (the only piece that genuinely depends on `i`)
      index_pass <- list()
      if(!is.null(index_args)){
        for(ind_v in index_args){
          index_pass[[ind_v]] <- dot_args[[ind_v]][[col_num]][i]
        }
      }

      if(is_summary[i]){
        draw_ci <- do.call(fn_summary, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = col_xlim,
               gp = summary_gp,
               nudge_y = nudge_y[col_num]),
          summary_extra,
          index_pass[names(index_pass) %in% args_summary]
        ))
      }else {
        draw_ci <- do.call(fn_ci, c(
          list(est = est[[col_num]][i],
               lower = lower[[col_num]][i],
               upper = upper[[col_num]][i],
               sizes = sizes[[col_num]][i],
               xlim = col_xlim,
               pch = ci_gp$pch[col_num],
               gp = ci_gpar,
               t_height = theme$ci$t_height,
               nudge_y = nudge_y[col_num]),
          ci_extra,
          index_pass[names(index_pass) %in% args_ci]
        ))
      }

      n_grobs <- n_grobs + 1L
      grob_list[[n_grobs]] <- draw_ci
      t_pos[n_grobs] <- i + 1L
      name_list[n_grobs] <- paste0("ci-", i, "-", current_col, "-", current_gp)
    }

    # One gtable_add_grob call per CI column instead of one per row
    if(n_grobs > 0L){
      grob_list <- grob_list[seq_len(n_grobs)]
      t_pos <- t_pos[seq_len(n_grobs)]
      name_list <- name_list[seq_len(n_grobs)]
      gt <- gtable_add_grob(gt, grob_list,
                            t = t_pos,
                            l = current_col,
                            b = t_pos,
                            r = current_col,
                            clip = "off",
                            name = name_list)
    }
  }

  gt
}

# Add the rows below the table and fill them: the x-axis, the arrows and the
# footnote, then the reference line, axis, vertical lines and arrows of each CI
# column
add_axis_grobs <- function(gt, data, ci_column, xlim, ticks_at, ticks_minor, ticks_digits,
                           x_trans, ref_line, xlab, arrow_lab, has_arrow, vert_line,
                           footnote, theme){

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
               xlab = xlab[[i]],
               x_trans = x_trans[i])
  })

  x_axht <- sapply(x_axis, function(x){
    ht <- Reduce(`+`, lapply(x$children, grobHeight))
    convertHeight(ht, unitTo = "mm", valueOnly = TRUE)
  })

  gt <- gtable_add_rows(gt, heights = unit(max(x_axht), "mm") + unit(.8, "lines"))

  # Prepare arrow object and row to put it
  if(any(has_arrow)){
    arrow_grob <- lapply(seq_along(xlim), function(i){
      if(!has_arrow[i])
        return(NULL)
      make_arrow(x0 = ref_line[i],
                 arrow_lab = arrow_lab[[i]],
                 arrow_gp = theme$arrow,
                 x_trans = x_trans[i],
                 col_width = convertWidth(gt$widths[ci_column[i]], "char", valueOnly = TRUE),
                 xlim = xlim[[i]])
    })

    lb_ht <- sapply(arrow_grob[has_arrow], function(x){
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
                          make_vline(x = ref_line[idx],
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
    if(!is.null(vert_line[[idx]]))
      gt <- gtable_add_grob(gt,
                            make_vline(x = vert_line[[idx]],
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
    if(has_arrow[idx])
      gt <- gtable_add_grob(gt, arrow_grob[[idx]],
                            t = nrow(gt), l = j,
                            b = nrow(gt), r = j,
                            clip = "off",
                            name = paste0("arrow-", j))

  }

  gt
}

# Add legend
add_legend_grob <- function(gt, theme, group_num){

  if(group_num > 1 && theme$legend$position != "none"){

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

  gt
}

# Add the title in a row above the table
add_title_grob <- function(gt, title, theme){

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

  gt
}
