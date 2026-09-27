
#' Set the labels of a forest plot
#'
#' Set the title, x-axis labels, footnote, arrow labels and legend text of a
#' forest plot. Their look, including the justification of the title, the
#' arrow type and the legend position, is set with \code{\link{forest_style}}.
#'
#' Arguments left out keep their current value, so the labels can be set in
#' several calls. \code{NULL} removes a label, or for the legend goes back to
#' the default text.
#'
#' This builds the plot again, so it must be used before the plot is edited
#' with \code{\link{edit_plot}}, \code{\link{add_text}},
#' \code{\link{insert_text}}, \code{\link{add_border}} or
#' \code{\link{add_grob}}.
#'
#' @param plot A forest plot object, see \code{\link{forest}}.
#' @param title The text for the title.
#' @param xlab X-axis labels, put under the x-axis. A vector with one label for
#' each CI column gives different labels to the columns, \code{NA} leaves a
#' column without a label.
#' @param footnote Footnote for the forest plot, aligned at the left bottom of
#' the plot. Please adjust the line length with line breaks to avoid overlap
#' with the arrows and/or x-axis.
#' @param arrow Labels for the arrows under the x-axis, a vector of length two
#' (left and right). A list with one pair of labels for each CI column gives
#' different arrows to the columns, \code{NA} leaves a column without arrows.
#' @param legend_title Title of the legend of a grouped forest plot, the
#' default is "Group".
#' @param legend_labels Legend labels, one for each group. Defaults to
#' "Group 1", "Group 2", ...
#'
#' @return A forest plot object.
#' @seealso \code{\link{forest}} \code{\link{forest_style}}
#' @example inst/examples/set-labs-example.R
#' @export
set_labs <- function(plot,
                     title,
                     xlab,
                     footnote,
                     arrow,
                     legend_title,
                     legend_labels){

  recipe <- recipe_to_update(plot, "set_labs")
  recipe <- update_labs(recipe, given_args(environment()))

  build_plot(recipe)
}

# Check the labels given in the list `args` and put them in the recipe, `NULL`
# removing a label. Used by `set_labs()`, and by `forest()` for the arguments
# that `set_labs()` replaces.
update_labs <- function(recipe, args){

  n_col <- length(recipe$ci_column)
  given <- names(args)

  if("title" %in% given){
    title <- args[["title"]]
    if(!is.null(title) && length(title) != 1)
      stop("title must be of length 1.")
    recipe$labs["title"] <- list(title)
  }

  if("xlab" %in% given){
    xlab <- args[["xlab"]]
    if(!is.null(xlab) && !length(xlab) %in% c(1, n_col))
      stop("xlab must be of length 1 or the same length as ci_column.")
    recipe$labs$xlab <- labels_by_column(xlab, n_col)
  }

  if("footnote" %in% given)
    recipe$labs["footnote"] <- list(args[["footnote"]])

  if("arrow" %in% given){
    arrow <- args[["arrow"]]
    if(inherits(arrow, "list")){
      if(length(arrow) != n_col)
        stop("arrow must have the same length as ci_column.")
      pair <- vapply(arrow, function(x){
        is.null(x) || is_na(x) || length(x) == 2
      }, FUN.VALUE = logical(1))
      if(!all(pair))
        stop("Elements in the arrow must be of length 2.")
    }else if(!is.null(arrow) && !is_na(arrow) && length(arrow) != 2){
      stop("Arrow label must be of length 2.")
    }
    recipe$labs$arrow <- by_column(arrow, n_col)
  }

  if("legend_title" %in% given)
    recipe$legend$title <- args[["legend_title"]]

  if("legend_labels" %in% given)
    recipe$legend$labels <- args[["legend_labels"]]

  recipe
}
