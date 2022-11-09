
#' Add border to cells
#'
#' Add border to any cells at any side.
#'
#' @param plot A forest plot object.
#' @param row A numeric value or vector indicating row number to add border.
#' This is corresponding to the data row number. Remember to account for any
#' text inserted. A border will be drawn to all rows if this is omitted.
#' @param col A numeric value or vector indicating the columns to add border.
#' A border will be drawn to all columns if this is omitted.
#' @param part The border will be added to \code{"body"} (default) or
#' \code{"header"}.
#' @param where Where to draw the border of the cell, possible values are
#' \code{"bottom"} (default), \code{"left"}, \code{"top"} and \code{"right"}
#' @param gp An object of class \code{"gpar"}, graphical parameter to be passed
#' to \code{\link[grid]{segmentsGrob}}.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[grid]{gpar}} \code{\link[grid]{segmentsGrob}} \code{\link[gtable]{gtable_add_grob}}
#' @rdname add_border
#' @export
add_border <- function(plot,
                       row = NULL,
                       col = NULL,
                       part = c("body", "header"),
                       where = c("bottom", "left", "top", "right"),
                       gp = gpar(lwd = 2.0)){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  part <- match.arg(part)
  where <- match.arg(where)

  l <- plot$layout

  # Header
  if(part == "header"){
    # Use all rows if row is NULL
    if(is.null(row))
      row <- unique(l$t[which(l$name == "colhead-fg")])
    else
      row <- row + min(l$t[which(l$name == "colhead-fg")]) - 1

    # Use all columns if col is NULL
    if(is.null(col))
      col <- unique(l$l[which(l$name == "colhead-fg")])
    else
      col <- col + min(l$l[which(l$name == "colhead-fg")]) - 1
  }else{
    # Use all rows if row is NULL
    if(is.null(row))
      row <- unique(l$t[which(l$name == "core-fg")])
    else
      row <- max(l$t[which(l$name == "colhead-fg")]) + row

    # Use all columns if col is NULL
    if(is.null(col))
      col <- unique(l$l[which(l$name == "core-fg")])
    else
      col <- col + min(l$l[which(l$name == "colhead-fg")]) - 1
  }

  pos_vec <- switch (where,
                     bottom = c(unit(0,"npc"), unit(0,"npc"), unit(1,"npc"), unit(0,"npc")),
                     left = c(unit(0,"npc"), unit(0,"npc"), unit(0,"npc"), unit(1,"npc")),
                     top = c(unit(0,"npc"), unit(1,"npc"), unit(1,"npc"), unit(1,"npc")),
                     right = c(unit(1,"npc"), unit(0,"npc"), unit(1,"npc"), unit(1,"npc")))

  for(i in seq_along(row)){
    for(j in seq_along(col)){
      seg_gb <- segmentsGrob(x0 = pos_vec[1],
                             y0 = pos_vec[2],
                             x1 = pos_vec[3],
                             y1 = pos_vec[4],
                             gp = gp,
                             name = "border")

      plot <- gtable_add_grob(plot, seg_gb,
                              t = row[i],
                              b = row[i],
                              l = col[j],
                              r = col[j],
                              clip = "off",
                              name = paste("border", row[i], col[j], where, sep = "-"))
    }
  }

  return(plot)

}


#' @title Add underline to cells
#' @description \code{\link{add_underline}} is a wrapper of \code{\link{add_border}} can be used to add
#' underline to cells.
#' @rdname add_border
#'
add_underline <- function(plot,
                          row = NULL,
                          col = NULL,
                          part = c("body", "header"),
                          gp = gpar(lwd = 2.0)){

  add_border(plot, row = row, col = col, part = part, where = "bottom", gp = gp)

}
