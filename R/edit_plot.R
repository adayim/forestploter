
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
#' @param part Part to edit, \code{"body"} (default), \code{"header"} or 
#' \code{"ci"} (confidence interval). This will not edit diamond shaped summary
#' CI, please set it by \code{\link{forest_theme}}.
#' @param which Which element to edit, text or background of the cell. This will
#' be ignored if \code{part = "ci"}
#' @param gp Pass \code{gpar} parameters, see \code{\link[grid]{gpar}}. It should
#' be passed as \code{gpar(col = "red")}.
#' 
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[grid]{gpar}} \code{\link[grid]{editGrob}} 
#' @export
#'
edit_plot <- function(plot,
                      row = NULL,
                      col = NULL,
                      part = c("body", "header", "ci"),
                      which = c("text", "background"),
                      gp){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  part <- match.arg(part)
  which <- match.arg(which)

  part <- switch(part,
                 body = "core",
                 header = "colhead",
                 ci = "ci")
  which <- switch(which,
                  text = "fg",
                  background = "bg")

  name_to_edit <- paste(part, which, sep = "-")

  l <- plot$layout

  # If body
  if(part == "core"){
    # Add number of header to the row
    if(!is.null(row))
      row <- row + max(l$b[which(l$name == "colhead-fg")])
    # Apply to whole body if missing row
    else
      row <- unique(l$b[which(l$name == "core-fg")])
  }else if(part == "colhead"){
    # If header, add header part
    if(!is.null(row))
      row <- row + min(l$b[which(l$name == "colhead-fg")]) - 1
    else
      row <- min(l$b[which(l$name == "colhead-fg")])
  }else{
    if(is.null(row) | is.null(col))
      stop("row and col must be difined for ci")

    # Generate name of the ci grob
    name_to_edit <- paste(part, apply(expand.grid(row, col), 1, paste, collapse="-"), sep = "-")
  }

  if(!is.null(col))
    col <- col + 1
  else
    col <- 2:max(l$r)

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
  plot
}
