
#' Edit forest plot
#'
#' This function is used to edit the graphical parameter of the forest plot.
#'
#' @param plot A forest plot object.
#' @param row A numeric value or vector indicating row number to edit in the
#' dataset. Will edit the whole row if left blank for the body. This will be
#' ignored if the \code{part} is "header".
#' @param col A numeric value or vector indicating column to edit in the dataset.
#'  Will edit the whole column if left blank.
#' @param part Part to edit, body (default) or header.
#' @param which Which element to edit, text or background of the cell.
#' @param gp Pass \code{gpar} parameters, see \code{\link[grid]{gpar}}. It should
#' be passed as \code{gpar(col = "red")}.
#' 
#' @return A \code{\link[gtable]{gtable}} object.
#'
#' @export
#'
edit_plot <- function(plot,
                      row = NULL,
                      col = NULL,
                      part = c("body", "header"),
                      which = c("text", "background"),
                      gp){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  data_dim <- attr(plot, "data.dim")

  part <- match.arg(part)
  which <- match.arg(which)

  part <- switch(part,
                 body = "core",
                 jeader = "colhead")
  which <- switch(which,
                  text = "fg",
                  background = "bg")

  name_to_edit <- paste(part, which, sep = "-")

  l <- plot$layout

  if(!is.null(row)){
    if(max(row) > data_dim[1])
      stop("row exceeds total data row number.")

    row <- row + max(l$b[which(l$name == "colhead-fg")])
  }


  if(!is.null(col)){
    if(col > data_dim[2])
      stop("col exceeds total data column number.")

    col <- col + 1
  }


  if(is.null(row))
    row <- 3:(data_dim[1]+2)

  if(is.null(col))
    col <- 2:(data_dim[2]+1)

  if(part == "header"){
    row <- 2
  }

  edit_cell(plot = plot, row = row, col = col, name = name_to_edit, gp = gp)

}

# Edit cell
edit_cell <- function(plot, row, col, name="core-fg", ...){
  l <- plot$layout
  ids <- which(l$t %in% row & l$l %in% col & l$name==name)
  for (id in ids){
    newgrob <- editGrob(plot$grobs[id][[1]], ...)
    plot$grobs[id][[1]] <- newgrob
  }
  plot
}
