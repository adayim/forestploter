
#' Forest plot style
#'
#' @description
#'
#' Set the look of a forest plot. Each part of the plot takes a
#' \code{\link[grid]{gpar}} object, and only the settings given are changed,
#' everything else keeps its default. A style can be passed to the \code{theme}
#' argument of \code{\link{forest}} or applied to a plot with
#' \code{\link{set_style}}, so the same style can be reused for many plots.
#' To change some settings of a plot and keep the rest, give them to
#' \code{\link{set_style}} instead.
#'
#' The text itself, such as the title or the legend labels, is set with
#' \code{\link{set_labs}}.
#'
#' @param base_size The size of text.
#' @param base_family The font family, the font of the device by default.
#' @param parse Whether text is read as plotmath expressions, see
#' \code{\link[grDevices]{plotmath}}. This applies to the title, x-axis labels,
#' footnote, arrow labels and legend labels; text in the table cells is parsed
#' with \code{parse} in \code{core} and \code{colhead} through \code{...}.
#' By default only the footnote is parsed, \code{TRUE} parses all of them where
#' the text is a valid expression and \code{FALSE} parses none.
#' @param ci Confidence intervals, \code{col}, \code{fill}, \code{lty},
#' \code{lwd} and \code{alpha} are used. Provide a vector for each group of a
#' grouped forest plot. \code{fill} is only used if \code{ci_pch} is within
#' \code{15:25} and \code{alpha} must be a single value; a small vertical line
#' marks the point estimate if it is not 1.
#' @param ci_pch Shape of the point estimation, reused for each group if a
#' single value is given.
#' @param ci_t_height The height of the T end of the confidence intervals. No T
#' end is drawn by default.
#' @param summary Diamond shaped summary confidence intervals, \code{col} and
#' \code{fill} are used.
#' @param ref_line Reference line, its position is set with
#' \code{ref_line} of \code{\link{forest}}.
#' @param vline Vertical lines, their positions are set with \code{vline} of
#' \code{\link{set_xaxis}}. \code{lwd}, \code{lty} and \code{col} can be
#' vectors with one value for each line.
#' @param xaxis X-axis line, tick marks and tick labels.
#' @param xlab X-axis labels.
#' @param xlab_adjust Align the x-axis labels to the reference line
#' \code{"refline"} (default) or to the center of the x-axis \code{"center"}.
#' @param title Title.
#' @param title_just The justification of the title, \code{"left"} (default),
#' \code{"right"} or \code{"center"}.
#' @param footnote Footnote.
#' @param arrow Arrows and their labels.
#' @param arrow_type Type of the arrow head, \code{"open"} (default) or
#' \code{"closed"}, see \code{\link[grid]{arrow}}.
#' @param arrow_length The length of the arrow head, a \code{\link[grid]{unit}}
#' or a number in inches. The default is \code{0.05} inches.
#' @param arrow_label_just Align the arrow labels to the starting point of the
#' arrows \code{"start"} (default) or to their ending point \code{"end"}.
#' @param legend Legend text.
#' @param legend_position Position of the legend, \code{"right"} (default),
#' \code{"top"}, \code{"bottom"} or \code{"none"} to hide the legend.
#' @param legend_ncol The number of columns of the legend, see
#' \code{\link[grid]{legendGrob}}.
#' @param legend_byrow Whether the rows of the legend are filled first, see
#' \code{\link[grid]{legendGrob}}.
#' @param body Text and background of the body of the table, a short form of
#' \code{core} in \code{...}: \code{col}, \code{fontsize}, \code{fontface},
#' \code{fontfamily}, \code{cex}, \code{lineheight} and \code{alpha} are used
#' for the text, \code{fill} for the background. A vector is recycled over the
#' rows.
#' @param header Text and background of the header of the table, a short form
#' of \code{colhead} in \code{...}, same as \code{body}.
#' @param fit How the plot uses the space it is drawn in, for example the size
#' given to \code{ggplot2::ggsave} or a panel of \code{patchwork}. With
#' \code{"none"} (default) the plot keeps its natural size, the size given by
#' \code{\link{get_wh}}, and is centred in the space. With \code{"width"} the
#' CI columns take the free width, in proportion to their natural width, and
#' become narrower when space is short. \code{"both"} also shares the free
#' height between the rows of the table. Text always keeps its size.
#' @param ... Settings passed on to the theme of the table, see
#' \code{\link[gridExtra]{tableGrob}}: \code{core} for the body of the table
#' and \code{colhead} for its header, each a list with \code{fg_params} for
#' the text and \code{bg_params} for the background. For example
#' \code{core = list(fg_params = list(hjust = 1, x = 0.9))} aligns the text of
#' the body to the right. \code{body} and \code{header} above are applied on
#' top of them, so settings given in both places come from \code{body} and
#' \code{header}. The border of a cell takes the colour of its fill, so that
#' there is no gap between the cells, unless \code{bg_params} of \code{core} or
#' \code{colhead} gives it a colour.
#'
#' @return A \code{forest_style} object.
#' @seealso \code{\link{set_style}} \code{\link{forest}} \code{\link{set_labs}}
#'  \code{\link[grid]{gpar}} \code{\link[gridExtra]{tableGrob}}
#' @example inst/examples/layout-example.R
#' @export
forest_style <- function(base_size = 12,
                         base_family = "",
                         parse = NULL,
                         ci = gpar(),
                         ci_pch = 15,
                         ci_t_height = NULL,
                         summary = gpar(),
                         ref_line = gpar(),
                         vline = gpar(),
                         xaxis = gpar(),
                         xlab = gpar(),
                         xlab_adjust = c("refline", "center"),
                         title = gpar(),
                         title_just = c("left", "right", "center"),
                         footnote = gpar(),
                         arrow = gpar(),
                         arrow_type = c("open", "closed"),
                         arrow_length = 0.05,
                         arrow_label_just = c("start", "end"),
                         legend = gpar(),
                         legend_position = c("right", "top", "bottom", "none"),
                         legend_ncol = 1,
                         legend_byrow = TRUE,
                         body = gpar(),
                         header = gpar(),
                         fit = c("none", "width", "both"),
                         ...){

  args <- c("base_size", "base_family", "parse", "ci", "ci_pch", "ci_t_height",
            "summary", "ref_line", "vline", "xaxis", "xlab", "xlab_adjust",
            "title", "title_just", "footnote", "arrow", "arrow_type",
            "arrow_length", "arrow_label_just", "legend", "legend_position",
            "legend_ncol", "legend_byrow", "body", "header", "fit")

  # `NULL` gives the default, which is left out for `parse` and `ci_t_height`
  style <- mget(args, envir = environment())
  for(nm in args){
    if(is.null(style[[nm]]))
      style[nm] <- list(eval(formals(forest_style)[[nm]]))
  }
  style <- drop_null(style)

  for(nm in intersect(names(style), c("ci", "summary", "ref_line", "vline",
                                      "xaxis", "xlab", "title", "footnote",
                                      "arrow", "legend", "body", "header"))){
    if(!inherits(style[[nm]], "gpar"))
      stop(sprintf("%s must be a gpar() object", nm))
  }

  choices <- list(xlab_adjust = c("refline", "center"),
                  title_just = c("left", "right", "center"),
                  arrow_type = c("open", "closed"),
                  arrow_label_just = c("start", "end"),
                  legend_position = c("right", "top", "bottom", "none"),
                  fit = c("none", "width", "both"))
  for(nm in intersect(names(style), names(choices)))
    style[[nm]] <- match_choice(style[[nm]], choices[[nm]], nm)

  if(!is.numeric(style$base_size) || length(style$base_size) != 1)
    stop("`base_size` must be a single number.")

  if(!is.null(style[["parse"]]) && !isTRUE(style[["parse"]]) && !isFALSE(style[["parse"]]))
    stop("`parse` must be NULL, TRUE or FALSE.")

  if(length(style[["ci_t_height"]]) > 1)
    stop("`ci_t_height` must be of length 1.")

  if(length(style[["ci"]]$alpha) > 1)
    stop("`alpha` of `ci` must be of length 1.")

  arrow_length <- style$arrow_length
  if(length(arrow_length) != 1 || !(is.numeric(arrow_length) || is.unit(arrow_length)))
    stop("`arrow_length` must be a single number or unit.")

  if(!is.numeric(style$legend_ncol) || length(style$legend_ncol) != 1)
    stop("`legend_ncol` must be a single number.")

  if(!isTRUE(style$legend_byrow) && !isFALSE(style$legend_byrow))
    stop("`legend_byrow` must be TRUE or FALSE.")

  table <- list(...)
  if(length(table) > 0){
    check_table_args(table)
    table <- drop_null(table)
  }
  if(length(table) > 0)
    style$table <- table

  class(style) <- "forest_style"
  style
}

#' Print a forest plot style
#'
#' Show the settings of a style that are not the default ones.
#'
#' @param x A style created with \code{\link{forest_style}}.
#' @param ... other arguments not used by this method
#' @return Invisibly returns the style.
#' @method print forest_style
#' @export
print.forest_style <- function(x, ...){

  default <- forest_style()
  given <- names(x)[!vapply(names(x), function(nm){
    identical(x[[nm]], default[[nm]])
  }, FUN.VALUE = logical(1))]

  cat("<forest_style>\n")

  if(length(given) == 0)
    cat(" the default style\n")

  for(nm in given)
    cat(" ", nm, ": ", style_value(x[[nm]]), "\n", sep = "")

  invisible(x)
}

# One setting of a style as a single line of text
style_value <- function(x){
  if(inherits(x, "gpar")){
    value <- vapply(x, function(v){
      paste(deparse(v), collapse = "")
    }, FUN.VALUE = character(1))
    return(paste0("gpar(", paste(names(x), value, sep = " = ", collapse = ", "), ")"))
  }

  if(is.list(x))
    return(paste0("a list of ", paste(names(x), collapse = ", ")))

  paste(deparse(x), collapse = "")
}

# Only the settings of the table can be given in `...` of `forest_style()`, so
# that a misspelt argument or one of `forest_theme()` is not silently ignored
check_table_args <- function(table){
  nms <- names(table)

  old <- intersect(nms, names(formals(forest_theme)))
  if(length(old) > 0)
    stop("Arguments of `forest_theme()` are not used by `forest_style()`: ",
         paste0("`", old, "`", collapse = ", "),
         ". See ?forest_theme for the matching settings of `forest_style()`.")

  unknown <- setdiff(nms, c("core", "colhead"))
  if(length(unknown) > 0)
    stop("Unknown arguments in `forest_style()`: ",
         paste0("`", unknown, "`", collapse = ", "),
         ". Only the table settings `core` and `colhead` can be given in `...`.")

  for(nm in nms){
    if(!is.null(table[[nm]]) && !is.list(table[[nm]]))
      stop("`", nm, "` must be a list, see `gridExtra::tableGrob()`.")
  }

  invisible()
}


#' Set the style of a forest plot
#'
#' Change the look of a forest plot. The settings given in \code{...} update
#' the current style of the plot: settings left out keep their current value,
#' a \code{\link[grid]{gpar}} is merged into the current one and \code{NULL}
#' goes back to the default. Like \code{\link{set_labs}}, this builds the plot
#' again, so it must be used before the plot is edited with
#' \code{\link{edit_plot}} and the other editing functions.
#'
#' @param plot A forest plot object, see \code{\link{forest}}.
#' @param style A style created with \code{\link{forest_style}}, or a theme
#' created with \code{\link{forest_theme}}, that replaces the current style of
#' the plot. The settings in \code{...} are applied on top of it.
#' \code{forest_style()} gives the default style.
#' @param ... Arguments of \code{\link{forest_style}} to change, for example
#' \code{title = gpar(col = "red")}, \code{fit = "width"} or
#' \code{core = list(...)}.
#'
#' @return A forest plot object.
#' @seealso \code{\link{forest_style}} \code{\link{forest}}
#' @example inst/examples/set-style-example.R
#' @export
set_style <- function(plot, style = NULL, ...){

  recipe <- recipe_to_update(plot, "set_style")

  if(!is.null(style) && !inherits(style, "forest_style") && !is_forest_theme(style))
    stop("style must be created with `forest_style()` or `forest_theme()`.")

  # Only the settings given are changed, checked by `forest_style()`
  update <- list(...)
  if(length(update) > 0 && (is.null(names(update)) || any(names(update) == "")))
    stop("Arguments in `...` must be named, e.g. `title = gpar(col = \"red\")`.")

  checked <- do.call(forest_style, drop_null(update))
  for(nm in names(update)){
    if(is.null(update[[nm]]))
      next
    update[nm] <- list(if(nm %in% c("core", "colhead")) checked$table[[nm]] else checked[[nm]])
  }

  # The legend text of a theme of `forest_theme()` now belongs to
  # `set_labs()`, so it is moved to the recipe first
  current <- recipe$style
  if(!is.null(style)){
    current <- style
    if(is_forest_theme(style)){
      old <- theme_to_style(style)
      recipe$legend <- modifyList(old$legend, as.list(recipe$legend))
      current <- old$style
    }
  }else if(!is.null(recipe$theme)){
    old <- theme_to_style(recipe$theme)
    recipe$legend <- modifyList(old$legend, as.list(recipe$legend))
    current <- old$style
  }

  if(is.null(current))
    current <- forest_style()

  recipe$theme <- NULL
  recipe$style <- merge_style(current, update)

  build_plot(recipe)
}


# Whether `x` is a theme created by `forest_theme()`
is_forest_theme <- function(x){
  is.list(x) && !inherits(x, "forest_style") &&
    all(c("ci", "legend", "tab_theme") %in% names(x))
}

# Update a style with the settings given to `set_style()`. Graphical
# parameters and table settings are merged, `NULL` goes back to the default and
# anything else is replaced.
merge_style <- function(style, update){
  for(nm in names(update)){
    val <- update[[nm]]
    if(nm %in% c("core", "colhead")){
      table <- style[["table"]]
      table[[nm]] <- if(is.null(val)) NULL else modifyList(as.list(table[[nm]]), val)
      style$table <- if(length(table) > 0) table else NULL
    }else if(is.null(val)){
      style[[nm]] <- NULL
    }else if(inherits(val, "gpar") && inherits(style[[nm]], "gpar")){
      style[[nm]] <- modifyList(style[[nm]], val)
    }else{
      style[nm] <- list(val)
    }
  }

  style
}

# Theme used to draw a plot, from the theme given to `forest()` or the style of
# the plot, together with the legend text of `set_labs()`
resolve_theme <- function(recipe){

  legend <- as.list(recipe$legend)

  if(!is.null(recipe$theme) && is.null(legend$labels)){
    theme <- recipe$theme
    if(!is.null(legend$title))
      theme$legend$name <- legend$title
    return(theme)
  }

  style <- recipe$style

  # The legend labels decide how the colours of a theme are recycled, so a
  # theme of `forest_theme()` is created again with them
  if(!is.null(recipe$theme)){
    old <- theme_to_style(recipe$theme)
    style <- old$style
    legend <- modifyList(old$legend, legend)
  }

  style_to_theme(style, legend)
}

# Drop the `NULL` elements of a list
drop_null <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

# Create a theme of `forest_theme()` from a style and the legend text. The
# legend labels decide how the settings of the confidence intervals are
# recycled over the groups.
style_to_theme <- function(style, legend = list()){

  args <- list()

  # Settings with the same name in `forest_theme()`
  same <- c("base_size", "base_family", "ci_pch", "xlab_adjust", "title_just",
            "arrow_type", "arrow_length", "arrow_label_just",
            "legend_position", "legend_ncol", "legend_byrow")
  for(nm in intersect(names(style), same))
    args[[nm]] <- style[[nm]]

  # Confidence intervals
  ci_args <- c(col = "ci_col", fill = "ci_fill", lty = "ci_lty",
               lwd = "ci_lwd", alpha = "ci_alpha")
  for(nm in intersect(names(style[["ci"]]), names(ci_args)))
    args[[ci_args[[nm]]]] <- style[["ci"]][[nm]]

  if(!is.null(style[["ci_t_height"]]))
    args$ci_Theight <- style[["ci_t_height"]]

  # Summary
  if(!is.null(style[["summary"]]$col))
    args$summary_col <- style[["summary"]]$col
  if(!is.null(style[["summary"]]$fill))
    args$summary_fill <- style[["summary"]]$fill

  # Vertical lines
  vl_args <- c(lwd = "vertline_lwd", lty = "vertline_lty", col = "vertline_col")
  for(nm in intersect(names(style[["vline"]]), names(vl_args)))
    args[[vl_args[[nm]]]] <- style[["vline"]][[nm]]

  # Other parts, `*_gp` of `forest_theme()`
  parts <- c(ref_line = "refline_gp", xaxis = "xaxis_gp", xlab = "xlab_gp",
             title = "title_gp", footnote = "footnote_gp", arrow = "arrow_gp",
             legend = "legend_gp")
  for(nm in intersect(names(style), names(parts))){
    if(!is.null(style[[nm]]))
      args[[parts[[nm]]]] <- style[[nm]]
  }

  if(isFALSE(style[["parse"]]))
    args$footnote_parse <- FALSE

  # Table, text settings go to the text and the fill to the background
  table <- style[["table"]]
  for(part in c("body", "header")){
    gp <- style[[part]]
    if(is.null(gp))
      next

    tab_part <- if(part == "body") "core" else "colhead"
    params <- list()

    fg <- unclass(gp)
    names(fg)[names(fg) == "font"] <- "fontface"
    fg$fontface <- unname(fg$fontface)
    fg <- fg[names(fg) %in% c("col", "fontsize", "fontface", "fontfamily",
                              "cex", "lineheight", "alpha")]
    if(length(fg) > 0)
      params$fg_params <- fg

    tab <- table[[tab_part]]
    if(is.null(tab))
      tab <- list()

    # The border of a cell takes the colour of its fill, so that there is no
    # gap between the cells, unless `core` or `colhead` gives it a colour
    if(!is.null(gp$fill)){
      params$bg_params <- list(fill = gp$fill)
      if(is.null(tab$bg_params$col))
        params$bg_params$col <- gp$fill
    }

    if(length(params) == 0)
      next

    table[[tab_part]] <- modifyList(tab, params)
  }
  args <- c(args, table)

  # Legend text
  if(!is.null(legend$title))
    args$legend_name <- legend$title

  # Vectors of confidence interval settings need a label for each group. The
  # default labels are filled in once the number of groups is known.
  labels <- legend$labels
  if(!is.null(labels)){
    args$legend_value <- labels
  }else{
    n_ci <- max(c(1, lengths(args[intersect(names(args),
                                              c("ci_pch", "ci_col", "ci_fill",
                                                "ci_lty", "ci_lwd"))])))
    if(n_ci > 1)
      args$legend_value <- rep("", n_ci)
  }

  theme <- do.call(forest_theme, args)

  if(is.null(labels))
    theme$legend$label <- ""

  # Settings of the vertical lines other than those taken by `forest_theme()`
  vl_extra <- setdiff(names(style[["vline"]]), names(vl_args))
  if(length(vl_extra) > 0)
    theme$vertline <- modifyList(theme$vertline, unclass(style[["vline"]])[vl_extra])

  if(isTRUE(style[["parse"]]))
    theme$parse_text <- TRUE

  theme
}

# Split a theme of `forest_theme()` into a style and the legend text
theme_to_style <- function(theme){

  base_size <- theme$base_size
  base_family <- theme$xaxis$fontfamily
  if(is.null(base_family))
    base_family <- ""

  # Font settings that only repeat the base font are left out, so that a later
  # change of `base_size` or `base_family` still reaches them
  drop_base <- function(x){
    if(!is.null(x$fontsize) && identical(as.numeric(x$fontsize), as.numeric(base_size)))
      x$fontsize <- NULL
    if(identical(x$fontfamily, base_family))
      x$fontfamily <- NULL
    x
  }

  # Confidence intervals. A fill that only repeats the colours of the groups is
  # the default and left out.
  ci <- theme$ci
  ci_gp <- list(col = ci$col, fill = ci$fill, lty = ci$lty, lwd = ci$lwd,
                alpha = unique(ci$alpha))
  if(length(ci$col) > 1 && identical(ci$fill, ci$col))
    ci_gp$fill <- NULL
  ci_gp <- structure(drop_null(ci_gp), class = "gpar")

  parse <- NULL
  if(isFALSE(theme$footnote$parse))
    parse <- FALSE
  else if(isTRUE(theme$parse_text))
    parse <- TRUE

  tab <- theme$tab_theme
  for(part in c("core", "colhead")){
    tab[[part]]$fg_params <- drop_base(tab[[part]]$fg_params)
  }

  style <- list(base_size = base_size,
                base_family = base_family,
                parse = parse,
                ci = ci_gp,
                ci_pch = ci$pch,
                ci_t_height = ci$t_height,
                summary = theme$summary,
                ref_line = drop_base(theme$refline),
                vline = drop_base(theme$vertline),
                xaxis = drop_base(theme$xaxis),
                xlab = drop_base(theme$xlab$gp),
                xlab_adjust = theme$xlab$just,
                title = drop_base(theme$title$gp),
                title_just = theme$title$just,
                footnote = drop_base(theme$footnote$gp),
                arrow = drop_base(theme$arrow$gp),
                arrow_type = theme$arrow$type,
                arrow_length = theme$arrow$length,
                arrow_label_just = theme$arrow$label_just,
                legend = drop_base(theme$legend$gp),
                legend_position = theme$legend$position,
                legend_ncol = theme$legend$ncol,
                legend_byrow = theme$legend$byrow,
                fit = "none",
                table = tab)
  class(style) <- "forest_style"

  legend <- list(title = theme$legend$name,
                 labels = theme$legend$label)

  # A blank label means the default labels
  if(identical(legend$labels, ""))
    legend$labels <- NULL

  list(style = style, legend = drop_null(legend))
}

# Turn text into plotmath expressions where it parses as a single expression,
# keep it as it is otherwise
parse_label <- function(x){
  if(!is.character(x))
    return(x)

  out <- lapply(x, function(txt){
    expr <- tryCatch(parse(text = txt), error = function(e) NULL)
    if(length(expr) == 1) expr[[1]] else txt
  })

  as.expression(out)
}
