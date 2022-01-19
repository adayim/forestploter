

#' Add underline to cells
#'
#' This function can be used to add underline to cells.
#'
#' @param plot A forest plot object.
#' @param row A numeric value or vector indicating row number to add underline.
#' This is corresponding to the data row number. Remember to account for any
#' text inserted. This will be ignored if the \code{part} is  "header" and the
#' underline will be drawn under the header column.
#' @param col A numeric value or vector indicating the columns to add underline.
#' @param part The underline will be added to \code{"body"} (default) or
#' \code{"header"}.
#' @param gp An object of class \code{"gpar"}, graphical parameter to be passed
#' to \code{\link[grid]{segmentsGrob}}.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' 
#' @export
#'
add_underline <- function(plot,
                          row = NULL,
                          col = NULL,
                          part = c("body", "header"),
                          gp = gpar(lwd = 2.0)){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  part <- match.arg(part)

  if(part == "body" & is.null(row))
    stop("row must be provided for the body.")

  l <- plot$layout

  # Header
  if(part == "header"){
    if(is.null(row))
      row <- max(l$b[which(l$name == "colhead-fg")])
    else
      row <- row + min(l$b[which(l$name == "colhead-fg")]) - 1
  }else{
    row <- max(l$b[which(l$name == "colhead-fg")]) + row
  }

  # Span to whole plot if col is missing
  if(is.null(col))
    col <- 2:max(l$r)
  else
    col <- 1 + col # Add 1 to account for padding of the plot

  for(i in seq_along(row)){

    seg_gb <- segmentsGrob(x0 = unit(0,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(1,"npc"),
                           y1 = unit(0,"npc"),
                           gp = gp,
                           name = paste("underline", row[i], sep = "-"))

    plot <- gtable_add_grob(plot, seg_gb,
                            t = row[i],
                            b = row[i],
                            l = min(col),
                            r = max(col),
                            name = "underline")
  }

  return(plot)


}
