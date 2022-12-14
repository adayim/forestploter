
#' Edit forest plot
#'
#' This function is used to edit the graphical parameter of text and background
#' of the forest plot.
#'
#' @param plot A forest plot object.
#' @param row A numeric value or vector indicating row number to edit in the
#' dataset. Will edit the whole row if left blank for the body. This will be
#' ignored if the \code{part} is "header".
#' @param col A numeric value or vector indicating column to edit in the dataset.
#'  Will edit the whole column if left blank.
#' @param part Part to edit, \code{"body"} (default) or \code{"header"}.
#' @param which Which element to edit, \code{"text"}, \code{"background"} or
#' \code{"ci"} (confidence interval). This will not edit diamond shaped summary
#' CI, please change it with \code{\link{forest_theme}}. Also, change in `ci` will
#' not have any impact on the legend.
#' @param gp Pass \code{gpar} parameters, see \code{\link[grid]{gpar}}. It should
#' be passed as \code{gpar(col = "red")}. For \code{which = "ci"}, please refer to
#' \code{\link{forest_theme}} \code{ci_*} parameters for the editable elements.
#' @param ... Other parameters to be passed to the grobs. For the \code{"text"}, please
#' see \code{\link[grid]{textGrob}} for details and \code{\link[grid]{rectGrob}}
#' for \code{"background"}. This will be ignored if \code{which = "ci"} as changing
#' parameters for the confidence interval are not allowed except for the graphical
#' parameters.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[grid]{gpar}} \code{\link[grid]{editGrob}} \code{\link{forest_theme}} 
#' \code{\link[grid]{textGrob}} \code{\link[grid]{rectGrob}}
#' @export
#'
edit_plot <- function(plot,
                      row = NULL,
                      col = NULL,
                      part = c("body", "header"),
                      which = c("text", "background", "ci"),
                      gp = gpar(),
                      ...){
  
  dots <- list(...)

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  part <- match.arg(part)
  which <- match.arg(which)

  part <- switch(part,
                 body = "core",
                 header = "colhead")
  which <- switch(which,
                  text = "fg",
                  background = "bg",
                  ci = "ci")

  if(which == "ci" & part != "core"){
    warning("`which=ci` but part is not set to body")
    part <- "core"
  }

  name_to_edit <- paste(part, which, sep = "-")

  l <- plot$layout

  # If body
  if(part == "core"){
    # For text and background
    if(which != "ci"){
      # Add number of header to the row
      if(!is.null(row))
        row <- row + max(l$b[which(l$name == "colhead-fg")])
      # Apply to whole body if missing row
      else
        row <- unique(l$b[which(l$name == "core-fg")])
    }else {
      # For CI
      if(is.null(row) | is.null(col))
        stop("row and col must be difined for ci")

      # Generate name of the ci grob
      name_to_edit <- paste(which, apply(expand.grid(row, col), 1, paste, collapse="-"), sep = "-")
    }

  }else{
    # If header, add header part
    if(!is.null(row))
      row <- row + min(l$b[which(l$name == "colhead-fg")]) - 1
    else
      row <- min(l$b[which(l$name == "colhead-fg")])
  }

  if(!is.null(col))
    col <- col + 1
  else
    col <- 2:max(l$r)

  if(which != "ci")
    edit_cell(plot = plot, row = row, col = col, name = name_to_edit, gp = gp, ...)
  else
    edit_cell(plot = plot, row = row, col = col, name = name_to_edit, gp = gp)

}

# Edit cell
edit_cell <- function(plot, row, col, name="core-fg", ...){
  l <- plot$layout
  if(any(grepl("ci", name)))
    ids <- which(grepl(paste(name, collapse = "|"), l$name))
  else
    ids <- which(l$t %in% row & l$l %in% col & grepl(paste(name, collapse = "|"), l$name))
  for (id in ids){
    newgrob <- editGrob(plot$grobs[id][[1]], ...)
    plot$grobs[id][[1]] <- newgrob
  }
  return(plot)
}
