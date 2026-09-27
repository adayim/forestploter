
#' Set the x-axis
#'
#' Set the limits, tick marks and scale of the x-axis of the CI columns, and
#' add vertical lines to them.
#'
#' Arguments left out keep their current value, so the axis can be set in
#' several calls, and \code{NULL} goes back to the default. A single value
#' applies to all CI columns. To give the columns different settings, provide
#' a list with one element for each CI column (a vector for
#' \code{ticks_digits} and \code{x_trans}), where \code{NA} leaves a column at
#' its default.
#'
#' This builds the plot again, so it must be used before the plot is edited
#' with \code{\link{edit_plot}}, \code{\link{add_text}},
#' \code{\link{insert_text}}, \code{\link{add_border}} or
#' \code{\link{add_grob}}.
#'
#' @param plot A forest plot object, see \code{\link{forest}}.
#' @param xlim Limits for the x-axis as a vector of length 2, i.e.
#' \code{c(low, high)}. By default the minimum and maximum of the lower and
#' upper values are used.
#' @param ticks_at Tick mark positions. By default, ticks are computed
#' automatically: \code{\link[base]{pretty}} for linear axes and a decade-aware
#' helper for log scales (e.g. \code{0.1, 1, 10, 100} for a wide log10 range).
#' @param ticks_digits Number of digits for the tick labels. If an integer is
#' given, for example \code{1L}, trailing zeros after the decimal mark are
#' dropped. Give a double, for example \code{1}, to keep them. By default the
#' number is calculated from the tick positions. Use a list to mix the two
#' between CI columns, as a vector makes them all double.
#' @param ticks_minor A numeric vector of positions to draw ticks without
#' labels. It can be a superset of \code{ticks_at} or disjoint from it.
#' @param x_trans Scale of the axis, one of \code{"none"} (default),
#' \code{"log"}, \code{"log2"} or \code{"log10"}. Use \code{"log"} if the
#' values are exponential, e.g. odds ratios or hazard ratios. The default
#' reference line of \code{\link{forest}} is 1 for log scales and 0 otherwise.
#' @param vline Numeric vector, positions of vertical lines drawn in addition to
#' the reference line, on the original scale of the x-axis. Their look is set
#' with \code{vertline} of \code{\link{forest_style}}. No lines are drawn by
#' default.
#'
#' @return A forest plot object.
#' @seealso \code{\link{forest}} \code{\link{set_labs}}
#'  \code{\link{forest_style}}
#' @example inst/examples/set-xaxis-example.R
#' @export
set_xaxis <- function(plot,
                      xlim,
                      ticks_at,
                      ticks_digits,
                      ticks_minor,
                      x_trans,
                      vline){

  recipe <- recipe_to_update(plot, "set_xaxis")
  recipe <- update_xaxis(recipe, given_args(environment()))

  build_plot(recipe)
}

# Check the x-axis settings given in the list `args` and put them in the
# recipe, `NULL` going back to the default. Used by `set_xaxis()`, and by
# `forest()` for the arguments that `set_xaxis()` replaces.
update_xaxis <- function(recipe, args){

  n_col <- length(recipe$ci_column)
  axis <- recipe$xaxis
  given <- names(args)

  if("xlim" %in% given){
    xlim <- args[["xlim"]]
    check_by_column(xlim, n_col, "xlim", function(x){
      is.numeric(x) && length(x) == 2 && !anyNA(x) && x[1] < x[2]
    }, "numeric and of length 2, with first element less than the second")
    axis$xlim <- by_column(xlim, n_col)
  }

  if("ticks_at" %in% given){
    check_by_column(args[["ticks_at"]], n_col, "ticks_at", is.numeric, "numeric")
    axis$ticks_at <- by_column(args[["ticks_at"]], n_col)
  }

  if("ticks_minor" %in% given){
    check_by_column(args[["ticks_minor"]], n_col, "ticks_minor", is.numeric, "numeric")
    axis$ticks_minor <- by_column(args[["ticks_minor"]], n_col)
  }

  if("ticks_digits" %in% given){
    ticks_digits <- args[["ticks_digits"]]
    if(!is.null(ticks_digits) && !is_na(ticks_digits)){
      if(!length(ticks_digits) %in% c(1, n_col))
        stop("ticks_digits must be length of 1 or same length as ci_column.")
      num <- vapply(ticks_digits, function(x){
        is_na(x) || (is.numeric(x) && length(x) == 1)
      }, FUN.VALUE = logical(1))
      if(!all(num))
        stop("ticks_digits must be numeric.")
    }
    axis$ticks_digits <- digits_by_column(ticks_digits, n_col)
  }

  if("x_trans" %in% given){
    x_trans <- args[["x_trans"]]
    if(is.null(x_trans))
      x_trans <- "none"
    x_trans[is.na(x_trans)] <- "none"
    if(!is.character(x_trans) ||
       !all(x_trans %in% c("none", "log", "log2", "log10")) ||
       !length(x_trans) %in% c(1, n_col))
      stop("x_trans must be in \"none\", \"log\", \"log2\", \"log10\" and of length 1 or the same length as ci_column.")
    axis$x_trans <- rep(x_trans, length.out = n_col)
  }

  if("vline" %in% given){
    check_by_column(args[["vline"]], n_col, "vline", is.numeric, "numeric")
    axis$vline <- by_column(args[["vline"]], n_col)
  }

  # Ticks are checked against the limits whenever either of them is set
  if(any(c("xlim", "ticks_at", "ticks_minor") %in% given)){
    for(i in seq_len(n_col)){
      lim <- axis$xlim[[i]]
      if(is.null(lim))
        next

      for(nm in c("ticks_at", "ticks_minor")){
        at <- axis[[nm]][[i]]
        if(!is.null(at) && (max(at) > max(lim) || min(at) < min(lim)))
          warning(nm, " is outside the xlim.")
      }
    }
  }

  recipe$xaxis <- axis

  # Messages of the last build, such as a confidence interval outside the
  # limits, are given again, as the axis they were about has changed
  if(length(given) > 0)
    recipe$seen <- character()

  recipe
}

# Check a setting given for all CI columns, or as a list with one element for
# each column where `NA` leaves the column at its default
check_by_column <- function(x, n_col, name, ok, what){
  if(is.null(x) || is_na(x))
    return(invisible())

  if(inherits(x, "list")){
    if(length(x) != n_col)
      stop(name, " must have the same length as ci_column.")

    valid <- vapply(x, function(val){
      is.null(val) || is_na(val) || ok(val)
    }, FUN.VALUE = logical(1))

    if(!all(valid))
      stop("The elements in ", name, " must be ", what, ".")
  }else if(!ok(x)){
    stop(name, " must be ", what, ".")
  }

  invisible()
}
