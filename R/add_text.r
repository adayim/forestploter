

#' Add text to forest plot
#'
#' This function can be used to add text to forest plot. The text can be span to
#' multiple rows and columns. The height of the row will be changed accordingly
#' if the text is added to only one row. The width of the text may exceeds the
#' columns provided if the text is too long.
#'
#' @param plot A forest plot object.
#' @param text A character or expression vector, see \code{\link[grid]{textGrob}}.
#' @param row Row to add the text, this will be ignored if the \code{part} is
#' "header".
#' @param col A numeric value or vector indicating the columns the text will be
#' added. The text will span over the column if a vector is given.
#' @param part Part to add text, body (default) or header.
#' @param just The justification of the text, \code{"center"} (default),
#' \code{"left"} or \code{"right"}.
#' @param gp An object of class \code{"gpar"}, this is the graphical parameter
#'  settings of the text. See \code{\link[grid]{gpar}}.
#' @param padding Padding of the text, default is \code{unit(1, "mm")}
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' 
#' @export
#'
add_text <- function(plot,
                     text,
                     row = NULL,
                     col = NULL,
                     part = c("body", "header"),
                     just = c("center", "left", "right"),
                     gp = gpar(),
                     padding = unit(1, "mm")){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  if(!is.unit(padding))
    padding <- unit(padding, "mm")

  part <- match.arg(part)
  just <- match.arg(just)

  # Row must be provided for the body
  if(part == "body" & is.null(row))
    stop("Row must be defined if the text is interting to body.")

  # Align text
  tx_x <- switch(just,
                 right = unit(1, "npc") - padding,
                 left  = padding,
                 center = unit(.5, "npc"))

  l <- plot$layout

  # Header
  if(part == "header"){
    if(!is.null(row))
      row <- row + min(l$b[which(l$name == "colhead-fg")]) - 1
    else
      row <- max(l$b[which(l$name == "colhead-fg")])
  }else{
    row <- max(l$b[which(l$name == "colhead-fg")]) + row
  }

  # Span to whole plot if col is missing
  if(is.null(col))
    col <- 2:max(l$r)
  else
    col <- 1 + col # Add 1 to account for padding of the plot

  txt_grob <- textGrob(label = text,
                       gp = gp,
                       x = tx_x,
                       just = just,
                       check.overlap = TRUE,
                       name = "custom-text.add")

  # Change height of the text is added to one row only,
  # and the height of the text is larger than then row
  if(length(row) == 1 ){
    txt_height <- convertHeight(grobHeight(txt_grob), "mm", valueOnly = TRUE)
    row_height <- convertHeight(plot$heights[row], "mm", valueOnly = TRUE)

    if(txt_height > row_height)
      plot$heights[row] <- grobHeight(txt_grob)

  }

  plot <- gtable_add_grob(plot, txt_grob,
                          t = min(row),
                          b = max(row),
                          l = min(col),
                          r = max(col),
                          clip = "off",
                          name = "text.add")

  return(plot)

}
