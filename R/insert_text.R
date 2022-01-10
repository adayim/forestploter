
#' Insert text to forest plot
#'
#' This function can be used to insert text to forest plot. Remember to adjust
#' for the row number if you have added text before, including header. This is
#' achieved by inserted new row(s) to the plot and will affect the row number.
#'
#' @param plot A forest plot object.
#' @param text A character or expression vector, see \code{\link[grid]{textGrob}}.
#' @param row Row to insert the text, this will be ignored if the \code{part} is
#' "header".
#' @param col A numeric value or vector indicating the columns the text will be
#' added. The text will span over the column if a vector is given.
#' @param part Part to insert text, body (default) or header.
#' @param just The justification of the text, \code{"center"} (default),
#' \code{"left"} or \code{"right"}.
#' @param before Indicating the text will be inserted before or after the row.
#' @param gp An object of class \code{"gpar"}, this is the graphical parameter
#'  settings of the text. See \code{\link[grid]{gpar}}.
#' @param padding Padding of the text, default is \code{unit(1, "mm")}
#' 
#' @return A \code{\link[gtable]{gtable}} object.
#'
#' @export
#'
insert_text <- function(plot,
                        text,
                        row = NULL,
                        col = NULL,
                        part = c("body", "header"),
                        just = c("center", "left", "right"),
                        before = TRUE,
                        gp = gpar(),
                        padding = unit(1, "mm")){

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")

  if(!inherits(gp, "gpar"))
    stop("gp must be a gpar object.")

  if(!is.unit(padding))
    padding <- unit(padding, "mm")

  data_dim <- attr(plot, "data.dim")

  part <- match.arg(part)
  just <- match.arg(just)

  # Align text
  tx_x <- switch(just,
                 right = unit(1, "npc") - padding,
                 left  = padding,
                 center = unit(.5, "npc"))

  l <- plot$layout

  # Header
  if(part == "header")
    row <- 2
  else
    row <- max(l$b[which(l$name == "colhead-fg")]) + row


  # Row must be provided for the body
  if(part == "body"){
    if(is.null(row))
      stop("Row must be defined if the text is interting to body.")

    if(length(row) != length(text))
      stop("text must have same legnth with row.")
  }

  # Span to whole plot if col is missing
  if(is.null(col))
    col <- 2:(ncol(plot) - 1)
  else
    col <- 1 + col # Add 1 to account for padding of the plot

  # Order row
  od_rw <- order(row)
  text <- text[od_rw]
  row <- row[od_rw]

  for(i in seq_along(row)){
    if(before)
      row[i] <- row[i] - 1 # Add 2 to account for padding of the plot and header
    else
      row[i] <- row[i]

    if(i != 1)
      row[1] <- row[i] + i - 1 # The row number will change after adding one row

    txt_grob <- textGrob(label = text[i],
                         gp = gp,
                         x = tx_x,
                         just = just,
                         check.overlap = TRUE,
                         name = "custom-text")

    plot <- gtable_add_rows(plot,
                            grobHeight(txt_grob) + 2*padding,
                            pos = row[i])

    plot <- gtable_add_grob(plot, txt_grob,
                            t = row[i] + 1,
                            b = row[i] + 1,
                            l = min(col),
                            r = max(col),
                            clip = "off")
  }

  return(plot)

}
