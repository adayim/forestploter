
#' Forest plot
#'
#' @description
#'
#' A data frame will be used for the basic layout of the forest plot.
#' Graphical parameters can be set using the \code{\link{forest_style}}
#' function.
#'
#' \code{forest} draws the table and the confidence intervals. The other parts
#' of the plot are added with functions that take the plot as their first
#' argument, so they can be chained with the pipe \code{|>}:
#'
#' \itemize{
#'   \item \code{\link{set_xaxis}} Limits, tick marks and scale of the x-axis,
#'   and vertical lines
#'   \item \code{\link{set_labs}} Title, x-axis labels, footnote, arrow labels
#'   and legend labels
#'   \item \code{\link{scale_sizes}} Point sizes scaled by study weights
#'   \item \code{\link{set_style}} Graphical parameters
#' }
#'
#' These functions build the plot again, so they must be used before the plot
#' is edited with \code{\link{edit_plot}}, \code{\link{add_text}},
#' \code{\link{insert_text}}, \code{\link{add_border}} or
#' \code{\link{add_grob}}. The plot stays a \code{\link[gtable]{gtable}} at
#' every step, and can be combined with other plots, e.g. with
#' \code{patchwork::wrap_elements}.
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
#' diamond. Values are used as they are, unless \code{\link{scale_sizes}} is
#' used to read them as study weights; useful values are roughly between
#' \code{0.2} and \code{1.5}, and a warning is given when the plot is drawn if
#' they are outside \code{0.1} to \code{2}.
#' @param ref_line X-axis coordinates of the reference line, the value of no
#' effect. If \code{NULL} (default), it is 1 if the x-axis is on a log scale
#' (see \code{\link{set_xaxis}}) and 0 otherwise. Provide an atomic vector if
#' different reference line for each \code{ci_column} is desired.
#' @param ci_column Column number of the data the CI will be displayed.
#' @param is_summary A logical vector indicating if the value is a summary value,
#' which will have a diamond shape for the estimate. With multiple groups the
#' diamonds are stacked in the same cell and the summary rows are made taller to
#' fit them, so a larger \code{nudge_y} may be wanted.
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
#' @param style Style of the forest plot created with
#' \code{\link{forest_style}}. A theme created with the superseded
#' \code{\link{forest_theme}} is also accepted. The style can also be set or
#' changed later with \code{\link{set_style}}.
#' @param ... Other arguments passed on to the \code{fn_ci} and
#' \code{fn_summary}, or named in \code{index_args}. An argument none of them
#' takes gives an error, as it would not be used.
#' The arguments of earlier versions are also accepted here, with a message the
#' first time each of them is used in a session: use \code{\link{set_xaxis}}
#' instead of \code{xlim}, \code{ticks_at}, \code{ticks_digits},
#' \code{ticks_minor}, \code{x_trans} and \code{vert_line},
#' \code{\link{set_labs}} instead of \code{arrow_lab}, \code{xlab},
#' \code{title} and \code{footnote}, and \code{style} instead of \code{theme}.
#'
#' @importFrom stats na.omit
#' @importFrom utils relist
#'
#'
#' @return A forest plot object, a \code{\link[gtable]{gtable}} of class
#' \code{forestplot}.
#' @seealso \code{\link[gtable]{gtable}} \code{\link[gridExtra]{tableGrob}}
#'  \code{\link{forest_style}} \code{\link{set_xaxis}} \code{\link{set_labs}}
#'  \code{\link{scale_sizes}} \code{\link{set_style}} \code{\link{make_boxplot}}
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
                   ref_line = NULL,
                   ci_column,
                   is_summary = NULL,
                   nudge_y = 0,
                   fn_ci = makeci,
                   fn_summary = make_summary,
                   index_args = NULL,
                   style = NULL,
                   ...){

  dot_args <- list(...)
  dot_names <- names(dot_args)
  if(is.null(dot_names))
    dot_names <- rep("", length(dot_args))

  # Arguments of earlier versions are still accepted, and taken out of `...` so
  # that they do not reach `fn_ci` and `fn_summary`
  old_args <- dot_args[dot_names %in% names(superseded_args)]
  dot_args <- dot_args[!dot_names %in% names(superseded_args)]
  signal_superseded(names(old_args))

  if("theme" %in% names(old_args)){
    if(!is.null(style))
      stop("Give the style of the plot to `style`, not to both `style` and `theme`.")
    style <- old_args[["theme"]]
  }

  # Check arguments
  args_ci <- names(formals(fn_ci))
  if(!all(c("est", "lower", "upper", "sizes", "xlim", "pch", "gp", "t_height", "nudge_y") %in% args_ci))
    stop("`fn_ci` must accept arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"pch\", \"gp\", \"t_height\", and \"nudge_y\".")

  args_summary <- names(formals(fn_summary))
  if(any(unlist(is_summary))){
    if(!all(c("est", "lower", "upper", "sizes", "xlim", "gp", "nudge_y") %in% args_summary))
    stop("`fn_summary` must accept arguments \"est\", \"lower\", \"upper\", \"sizes\", \"xlim\", \"gp\", and \"nudge_y\".")
  }

  # Anything else in `...` is only used if `fn_ci`, `fn_summary` or
  # `index_args` take it, so a misspelt argument is reported instead of being
  # dropped silently
  if(length(dot_args) > 0 && !"..." %in% c(args_ci, args_summary)){
    unknown <- setdiff(names(dot_args), c(args_ci, args_summary, index_args, ""))
    if(length(unknown) > 0)
      stop("Unknown arguments: ", paste0("`", unknown, "`", collapse = ", "),
           ". They are not used by `fn_ci`, `fn_summary` or `index_args`, see ?forest.")
  }

  check_errors(data = data, est = est, lower = lower, upper = upper, sizes = sizes,
               ref_line = ref_line, ci_column = ci_column, is_summary = is_summary)

  # Values that can differ between CI columns are kept with one element per CI
  # column, `NULL` meaning not set
  n_col <- length(ci_column)

  # A style of `forest_style()` is kept apart from a theme of `forest_theme()`
  theme <- NULL
  if(!inherits(style, "forest_style")){
    theme <- style
    style <- NULL
  }

  # The recipe holds everything the plot is built from. An unset reference line
  # stays unset, so that it follows the scale set with `set_xaxis()`.
  recipe <- list(
    data = data,
    est = est,
    lower = lower,
    upper = upper,
    sizes = sizes,
    size_scale = NULL,
    ref_line = ref_line,
    ci_column = ci_column,
    is_summary = is_summary,
    ci = list(nudge_y = nudge_y,
              fn_ci = fn_ci,
              fn_summary = fn_summary,
              index_args = index_args,
              dots = dot_args),
    xaxis = list(xlim = vector("list", n_col),
                 ticks_at = vector("list", n_col),
                 ticks_minor = vector("list", n_col),
                 ticks_digits = vector("list", n_col),
                 x_trans = rep("none", n_col),
                 vline = vector("list", n_col)),
    labs = list(title = NULL,
                xlab = vector("list", n_col),
                footnote = NULL,
                arrow = vector("list", n_col)),
    legend = list(),
    theme = theme,
    style = style,
    seen = character()
  )
  class(recipe) <- "forest_recipe"

  # The arguments of earlier versions go through the same checks as
  # `set_xaxis()` and `set_labs()`
  names(old_args)[names(old_args) == "vert_line"] <- "vline"
  names(old_args)[names(old_args) == "arrow_lab"] <- "arrow"
  recipe <- update_xaxis(recipe, old_args[names(old_args) %in% c("xlim", "ticks_at",
                                                                "ticks_digits", "ticks_minor",
                                                                "x_trans", "vline")])
  recipe <- update_labs(recipe, old_args[names(old_args) %in% c("title", "xlab",
                                                               "footnote", "arrow")])

  build_plot(recipe)

}

#' Draw plot
#'
#' Print or draw forestplot.
#'
#' @param x forestplot to display
#' @param autofit If true, the page is shared equally between the columns and
#' between the rows of the plot. This will be deprecated, use \code{fit} of
#' \code{\link{forest_style}} instead, which also works with
#' \code{ggplot2::ggsave} and \code{patchwork}.
#' @param ... other arguments not used by this method
#' @return Invisibly returns the original forestplot.
#' @rdname print.forestplot
#' @method print forestplot
#' @export
print.forestplot <- function(x, autofit = FALSE, ...){

  if(autofit){
    if(!exists("autofit", envir = superseded_seen, inherits = FALSE)){
      message("autofit will be deprecated, use fit of forest_style() instead.")
      assign("autofit", TRUE, envir = superseded_seen)
    }

    # Auto fit the page, in place of the `fit` of the style
    x$widths <- unit(rep(1/ncol(x), ncol(x)), "npc")
    x$heights <- unit(rep(1/nrow(x), nrow(x)), "npc")
    attr(x, "forest_fit") <- "none"
  }

  grid.newpage()
  grid.draw(x)

  invisible(x)
}

#' @method plot forestplot
#' @rdname print.forestplot
#' @export
plot.forestplot <- print.forestplot

# The plot takes the space it is drawn in when the `fit` of its style asks for
# it. This is done when the plot is drawn, as only then the space is known,
# which covers printing, `ggsave()` and patchwork alike. The plot itself keeps
# its natural size.
#' @export
makeContext.forestplot <- function(x){
  fit <- attr(x, "forest_fit", exact = TRUE)
  if(is.null(fit))
    fit <- get_recipe(x)$style[["fit"]]

  if(!is.null(fit) && fit != "none")
    x <- fit_layout(x, fit)

  NextMethod()
}

# Share the free space between the CI columns in proportion to their natural
# width, and with `fit = "both"` the free height between the rows of the table.
# CI columns become narrower when space is short, down to one line of text;
# other columns and rows keep their size. The columns and rows are found from
# the layout, so that edited plots fit as well.
fit_layout <- function(x, fit){

  l <- x$layout
  ci_col <- sort(unique(l$l[grepl("^xaxis-", l$name)]))
  if(length(ci_col) == 0)
    return(x)

  nat_w <- convertWidth(x$widths, "mm", valueOnly = TRUE)
  free_w <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE) - sum(nat_w)
  min_w <- convertWidth(unit(1, "lines"), "mm", valueOnly = TRUE)
  ci_w <- nat_w[ci_col] + free_w * nat_w[ci_col] / sum(nat_w[ci_col])
  x$widths[ci_col] <- unit(pmax(ci_w, min_w), "mm")

  # Arrows are laid out again for the width of their column, and tick labels
  # that would overlap in a narrow column are left out
  for(i in which(grepl("^arrow-", l$name))){
    arrow_args <- x$grobs[[i]]$arrow_args
    if(is.null(arrow_args))
      next
    arrow_args$col_width <- convertWidth(x$widths[l$l[i]], "char", valueOnly = TRUE)
    x$grobs[[i]] <- do.call(make_arrow, arrow_args)
  }

  for(i in which(grepl("^xaxis-", l$name)))
    x$grobs[[i]] <- editGrob(x$grobs[[i]], "label", check.overlap = TRUE)

  if(fit == "both"){
    # The rows between the header and the x-axis, rows inserted included
    body <- seq(max(l$b[grepl("^colhead-", l$name)]) + 1,
                min(l$t[grepl("^xaxis-", l$name)]) - 1)
    nat_h <- convertHeight(x$heights, "mm", valueOnly = TRUE)
    free_h <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE) - sum(nat_h)
    if(free_h > 0)
      x$heights[body] <- unit(nat_h[body] + free_h / length(body), "mm")
  }

  x
}

# Warnings held back while the plot was built are given when it is first drawn,
# which covers printing, `grid.draw()`, `ggsave()` and patchwork alike.
#' @export
makeContent.forestplot <- function(x){
  state <- get_recipe(x)$state

  if(is.environment(state) && !isTRUE(state$warned) && length(state$warnings) > 0){
    state$warned <- TRUE
    for(msg in state$warnings)
      warning(msg, call. = FALSE)
  }

  NextMethod()
}


# A forest plot is a built gtable that carries the inputs it was built from
# (the recipe) in `attr(plot, "forest_recipe")`. `set_xaxis()`, `set_labs()`,
# `scale_sizes()` and `set_style()` change the recipe and build the plot again,
# so they have to be used before the gtable is edited.

# Build a plot from its recipe and attach the recipe to it.
#
# Messages and warnings already given by the previous build of the same plot
# are not repeated, so a chain of functions shows each of them once.
build_plot <- function(recipe){

  recipe$state <- new.env(parent = emptyenv())

  shown <- character()
  muffle <- function(cond, restart){
    msg <- conditionMessage(cond)
    shown <<- c(shown, msg)
    if(msg %in% recipe$seen)
      invokeRestart(restart)
  }

  plot <- withCallingHandlers(
    draw_forest(recipe),
    message = function(m) muffle(m, "muffleMessage"),
    warning = function(w) muffle(w, "muffleWarning")
  )
  recipe$seen <- shown

  set_recipe(plot, recipe)
}

# Recipe of a plot that is going to be built again
recipe_to_update <- function(plot, fn){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  recipe <- get_recipe(plot)

  if(is.null(recipe))
    stop("`", fn, "()` needs a plot created by `forest()` of forestploter ",
         "1.2.0 or later, please create the plot again.")

  if(isTRUE(recipe$edited) || !identical(recipe$sig, plot_sig(plot)))
    stop("`", fn, "()` builds the plot again, so it must be used before the ",
         "plot is edited, e.g. with `edit_plot()`, `add_text()`, ",
         "`insert_text()`, `add_border()`, `add_grob()` or gtable functions. ",
         "Create the plot again with `forest()` to change this.")

  recipe
}

# The table is the slowest part of a build, so the last one is kept and reused
# while the data and the table theme stay the same. It is kept here rather than
# in the recipe, so that saved plots do not carry a second copy of it.
table_cache <- new.env(parent = emptyenv())

cached_table <- function(data, tab_theme){
  if(!is.null(table_cache$table) &&
     identical(table_cache$data, data) &&
     identical(table_cache$tab_theme, tab_theme))
    return(table_cache$table)

  table <- tableGrob(data, theme = tab_theme, rows = NULL)

  table_cache$table <- table
  table_cache$data <- data
  table_cache$tab_theme <- tab_theme
  table_cache$n_built <- if(is.null(table_cache$n_built)) 1 else table_cache$n_built + 1

  table
}

# Get the recipe of a plot, `NULL` if it has none
get_recipe <- function(plot){
  attr(plot, "forest_recipe", exact = TRUE)
}

# Attach a recipe to a plot, together with a fingerprint of the plot as it is
set_recipe <- function(plot, recipe){
  recipe$sig <- plot_sig(plot)
  attr(plot, "forest_recipe") <- recipe
  plot
}

# Mark a plot as edited by the editing functions, as the plot cannot be built
# again without losing the edits
mark_edited <- function(plot){
  recipe <- get_recipe(plot)
  if(!is.null(recipe)){
    recipe$edited <- TRUE
    attr(plot, "forest_recipe") <- recipe
  }
  plot
}

# Fingerprint of a plot, used to tell whether the gtable has been changed
# since it was built. Units and grobs are kept as text, as the units of the
# table hold the grobs they are measured from.
plot_sig <- function(plot){
  list(layout = plot$layout,
       widths = as.character(plot$widths),
       heights = as.character(plot$heights),
       grobs = vapply(plot$grobs, function(x) paste(class(x)[1], x$name),
                      FUN.VALUE = character(1)))
}

# Arguments of `forest()` that moved to the pipe functions, with the function
# that replaces them
superseded_args <- c(xlim = "set_xaxis()",
                     ticks_at = "set_xaxis()",
                     ticks_digits = "set_xaxis()",
                     ticks_minor = "set_xaxis()",
                     x_trans = "set_xaxis()",
                     vert_line = "set_xaxis()",
                     arrow_lab = "set_labs()",
                     xlab = "set_labs()",
                     title = "set_labs()",
                     footnote = "set_labs()",
                     theme = "style of forest()")

# Arguments of `forest()` already reported in this session
superseded_seen <- new.env(parent = emptyenv())

# Give a message the first time a superseded argument of `forest()` is used in
# a session, in the same form as the old arguments of `forest_theme()`
signal_superseded <- function(args){
  args <- setdiff(args, ls(superseded_seen))
  if(length(args) == 0)
    return(invisible())

  for(fn in unique(superseded_args[args])){
    message(paste(args[superseded_args[args] == fn], collapse = ", "),
            " will be deprecated, use ", fn, " instead.")
  }

  for(arg in args)
    assign(arg, TRUE, envir = superseded_seen)

  invisible()
}

# Reference line used when none is given, 1 on a log scale and 0 otherwise
default_ref_line <- function(x_trans){
  ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0)
}

# Keep a warning to give when the plot is first drawn
defer_warning <- function(recipe, ...){
  recipe$state$warnings <- c(recipe$state$warnings, paste0(...))
}

# Arguments given to the pipe function calling this, as a list. Arguments left
# out are not in the list, so that they keep their current value, while a
# `NULL` given is kept, as it goes back to the default.
given_args <- function(env){
  args <- setdiff(names(formals(sys.function(sys.parent()))), "plot")
  given <- args[!vapply(args, function(x){
    eval(call("missing", as.name(x)), env)
  }, FUN.VALUE = logical(1))]

  mget(given, envir = env)
}

# Check a setting against the values it can take, as `match.arg` does but with
# the name of the setting in the message
match_choice <- function(value, choices, name){
  if(identical(value, choices))
    return(choices[1])

  ind <- if(is.character(value) && length(value) == 1) pmatch(value, choices) else NA_integer_
  if(is.na(ind))
    stop("`", name, "` must be one of ",
         paste0("\"", choices, "\"", collapse = ", "), ".")

  choices[ind]
}

# Whether `x` is a single missing value, used for a CI column left at its
# default
is_na <- function(x){
  is.atomic(x) && length(x) == 1 && is.na(x)
}

# Spread a value over CI columns: an atomic value is used for every column and
# a list is taken as one element per column. `NULL` and `NA` mean not set.
by_column <- function(x, n_col){
  if(is.null(x) || is_na(x))
    return(vector("list", n_col))

  if(!inherits(x, "list"))
    return(rep(list(x), n_col))

  lapply(x, function(val){
    if(is.null(val) || is_na(val)) NULL else val
  })
}

# One label for each CI column from a vector, `NULL` for a column without one
labels_by_column <- function(x, n_col){
  if(is.null(x) || is_na(x))
    return(vector("list", n_col))

  if(length(x) == 1)
    x <- rep(x, n_col)

  lapply(seq_len(n_col), function(i){
    if(is_na(x[i])) NULL else x[i]
  })
}

# Number of digits for each CI column from a vector or a list, keeping whether
# each is an integer
digits_by_column <- function(x, n_col){
  if(is.null(x) || is_na(x))
    return(vector("list", n_col))

  x <- as.list(rep(x, length.out = n_col))
  lapply(x, function(val){
    if(is.null(val) || is_na(val)) NULL else val
  })
}
