
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
#' @param sizes Size of the point estimation box, can be a vector or a list.
#' The value is a multiple of one line of text, so \code{1} draws a point as tall
#' as the \code{base_size} of the theme. The same scale applies to the summary
#' diamond. Values are used as they are unless \code{size_method} is set; useful
#' values are roughly between \code{0.2} and \code{1.5}, and a warning is given
#' outside \code{0.1} to \code{2}.
#' @param size_method How to turn \code{sizes} into point sizes. The default
#' \code{"none"} uses the values as they are. Any other value reads \code{sizes}
#' as study weights and takes their square root, so that the \emph{area} of the
#' point is proportional to the weight, before mapping onto \code{size_range}.
#' \code{"range"} puts the smallest weight on \code{size_range[1]} and the
#' largest on \code{size_range[2]}, as \code{metafor::forest.rma} does with its
#' \code{plim}. \code{"proportional"} keeps the areas proportional to the weights
#' and only clamps the smallest points up, as \code{meta::forest.meta} does.
#' Weights are scaled jointly across all groups and CI columns so that the areas
#' stay comparable between them; scale by hand if per-column control is wanted.
#' Rows flagged by \code{is_summary} are held out of the scaling and drawn at
#' \code{size_range[2]}, since a pooled total is not comparable with a study
#' weight. This follows \code{meta}, whose pooled rows carry no study weight and
#' end up the size of the largest study square, and \code{metafor}, which sizes
#' its summary polygon from \code{efac} rather than from the weights.
#' @param size_range Numeric vector of length 2 giving the smallest and largest
#' point size \code{size_method} may produce. Ignored if \code{size_method} is
#' \code{"none"}.
#' @param ref_line X-axis coordinates of zero line, default is 1. Provide an atomic
#'  vector if different reference line for each \code{ci_column} is desired.
#' @param vert_line Numerical vector, add additional vertical line at given value.
#' Provide a list of numerical vector element if different vertical line for each
#'  \code{ci_column} is desired.
#' @param ci_column Column number of the data the CI will be displayed.
#' @param is_summary A logical vector indicating if the value is a summary value,
#' which will have a diamond shape for the estimate. With multiple groups the
#' diamonds are stacked in the same cell and the summary rows are made taller to
#' fit them, so a larger \code{nudge_y} may be wanted.
#' @param xlim Limits for the x axis as a vector of length 2, i.e. c(low, high). It
#' will take the minimum and maximum of the lower and upper value if not provided.
#' This will apply to all CI columns if provided, and will be calculated automatically
#' for each column if not provided. This should be a list with the same length of
#' \code{ci_column} if different \code{xlim} for different column is desired.
#' @param ticks_at Set X-axis tick-mark positions. Applies to all CI columns if
#' a single vector is provided, or supply a list of vectors for column-specific
#' ticks. When omitted, ticks are computed automatically: \code{\link[base]{pretty}}
#' for linear axes and a decade-aware log-pretty helper for \code{x_trans} in
#' \code{"log"}, \code{"log2"}, or \code{"log10"} (e.g. \code{0.1, 1, 10, 100}
#' for a wide log10 range). The auto-tick output is rarely a perfect substitute
#' for a hand-chosen set, so supply this argument when you need exact control.
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
#' @param nudge_y Vertical adjustment to nudge groups by, must be within 0 to 1.
#' Defaults to \code{0}; for grouped forest plots a value of \code{0} is bumped
#' to \code{0.1} automatically so that group CIs do not overplot. Set explicitly
#' to override.
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
#' @importFrom utils relist
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
                   size_method = c("none", "range", "proportional"),
                   size_range = c(0.2, 0.8),
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

  size_method <- match.arg(size_method)

  # Check arguments
  args_ci <- names(formals(fn_ci))
  if(!all(c("est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y") %in% args_ci))
    stop("`fn_ci` must accept arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"pch\", \"gp\", \"t_height\", and \"nudge_y\".")

  args_summary <- names(formals(fn_summary))
  if(any(unlist(is_summary))){
    if(!all(c("est", "lower", "upper", "sizes", "xlim", "gp", "nudge_y") %in% args_summary))
    stop("`fn_summary` must accept arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"gp\", and \"nudge_y\".")
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

  # Wrap ticks_minor into list form first so it's safe to fall back to
  if(!is.null(ticks_minor) && !inherits(ticks_minor, "list"))
    ticks_minor <- rep(list(ticks_minor), length(ci_column))

  if(is.null(ticks_at)){
    ticks_at <- ticks_minor
  }else if(!inherits(ticks_at, "list")){
    ticks_at <- rep(list(ticks_at), length(ci_column))
  }

  if(is.null(ticks_minor))
    ticks_minor <- ticks_at

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

  # Read `sizes` as weights and scale them. Normalised jointly across all groups
  # and CI columns so the areas stay comparable between them; use the default
  # `size_method = "none"` and scale by hand if per-column control is wanted.
  #
  # Summary rows are held out. Their weight is a pooled total rather than a
  # study weight, so leaving them in would drag the whole normalisation towards
  # the summary and squash the studies into a narrow band. They are then drawn
  # at the top of the range, which is what `meta` does: its pooled rows have no
  # study weight, so they fall to the fixed `1 * squaresize`, the same size as
  # the largest study square. `metafor` likewise sizes its summary polygon from
  # `efac` alone and never from the weights.
  if(size_method != "none"){
    summ_row <- if(is.null(is_summary)) rep(FALSE, nrow(data)) else is_summary

    if(all(lengths(sizes) == length(summ_row)))
      summ_flat <- rep(summ_row, length(sizes))
    else
      summ_flat <- rep(FALSE, length(unlist(sizes)))

    size_flat <- unlist(sizes)
    keep <- !is.na(size_flat)
    size_flat[summ_flat] <- NA

    size_flat <- scale_sizes(size_flat, range = size_range, method = size_method)
    size_flat[summ_flat & keep] <- size_range[2]

    sizes <- relist(size_flat, sizes)
  }

  # Warn on sizes that will not render sensibly. Done here rather than in
  # `check_errors` so that the values checked are the ones actually drawn, not
  # the raw weights handed to `size_method`.
  size_flat <- unlist(sizes)
  if(any(size_flat < 0.1, na.rm = TRUE) || any(size_flat > 2, na.rm = TRUE))
    warning("`sizes` outside the usual range of 0.1 to 2: values below 0.1 draw ",
            "smaller than a point and are invisible, values above 2 are taller ",
            "than the row and overlap neighbouring rows. Weight-based sizes ",
            "typically fall between 0.2 and 1.5; see the `size_method` argument.")

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

  nudge_y <- rep(nudge_y, each = length(ci_column))

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
  if(group_num > 1){
    gap_npc <- min(diff(sort(unique(nudge_y))))
    row_pt <- 0.72 * theme$base_size + core_padding_bigpts(theme)
    max_size <- max(size_flat, na.rm = TRUE)
    if(max_size * theme$base_size > 2 * gap_npc * row_pt)
      warning("Grouped confidence intervals are likely to overlap: a point of ",
              "`sizes` ", signif(max_size, 3), " is more than twice the gap ",
              "between groups. Increase `nudge_y` or reduce `sizes`.")
  }

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
  if(is.null(ticks_digits) && !is.null(ticks_at)){
    if(is.list(ticks_at))
      ticks_digits <- sapply(ticks_at, function(x){
        max(count_decimal(x))
      })
    else
      ticks_digits <- max(count_decimal(ticks_at))

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

  # Stacked group diamonds need room. A diamond of height `s` big points sitting
  # at npc offset `o` fits only if the row is at least `(s/2) / (0.5 - |o|)`
  # tall, so grow the summary rows to what the offsets actually need instead of
  # a flat doubling. `unit.pmax` resolves lazily at draw time, so the natural
  # row height never has to be measured here.
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


  # Do not clip text
  gt$layout$clip <- "off"

  # Column index
  col_indx <- rep_len(1:length(ci_column), length(ci_col_list))

  # Draw CI
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
    ci_gp <- gpar(lty = lty_list[col_num],
                  lwd = lwd_list[col_num],
                  col = color_list[col_num],
                  fill = fill_list[col_num],
                  alpha = alpha_list[col_num],
                  fontsize = theme$base_size)
    if(!is.null(user_gp))
      ci_gp <- modifyList(user_gp, ci_gp)

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
        warning("Missing lower and/or upper limit on column", current_col, " row ", i)
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
               pch = pch_list[col_num],
               gp = ci_gp,
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
                          make_vert_line(x = ref_line[idx],
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
                            make_vert_line(x = vert_line[[idx]],
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
