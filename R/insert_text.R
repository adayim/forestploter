
#' Insert text to forest plot
#'
#' This function can be used to insert text to forest plot. Remember to adjust
#' for the row number if you have added text before, including header. This is
#' achieved by inserted new row(s) to the plot and will affect the row number.
#' A text vector can be inserted to multiple columns or rows.
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

  part <- match.arg(part)
  just <- match.arg(just)

  # Row must be provided for the body
  if(part == "body" & is.null(row))
    stop("Row must be defined if the text is interting to body.")

  # Check text length 
  if(length(text) > 1 && length(row) != length(text) && length(col) != length(text))
    stop("text must have same legnth with row or col.")

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

  # If the text will be put in columns
  if(!is.null(col) && length(text) == length(col) && length(row) == 1)
    by_col <- TRUE
  else
    by_col <- FALSE

  # Span to whole plot if col is missing
  if(is.null(col))
    col <- 2:(ncol(plot) - 1)
  else
    col <- 1 + col # Add 1 to account for padding of the plot

  # Order row
  if(!by_col){
    od_rw <- order(row)
    text <- text[od_rw]
    row <- row[od_rw]
  }

  nam_text <- paste(ifelse(part == "header", "colhead", "core"), "fg", sep = "-")

  for(i in seq_along(row)){
    if(before)
      row[i] <- row[i] - 1 # Account for padding of the plot and header

    if(i != 1)
      row[1] <- row[i] + i - 1 # The row number will change after adding one row

    if(by_col){
      # Get maximum height of text and add a row
      max_height <- max(convertHeight(stringHeight(text), "mm", valueOnly = TRUE))
      plot <- gtable_add_rows(plot, unit(max_height, "mm") + 2*padding, pos = row[i])

      for(j in seq_along(col)){
        txt_grob <- textGrob(label = text[j],
                             gp = gp,
                             x = tx_x,
                             just = just,
                             check.overlap = TRUE,
                             name = "custom-text.insert")

        plot <- gtable_add_grob(plot, txt_grob,
                                t = row[i] + 1,
                                b = row[i] + 1,
                                l = col[j],
                                r = col[j],
                                clip = "off",
                                name = nam_text)
      }

    }else{
      txt_grob <- textGrob(label = text[i],
                           gp = gp,
                           x = tx_x,
                           just = just,
                           check.overlap = TRUE,
                           name = "custom-text.insert")

      plot <- gtable_add_rows(plot,
                              grobHeight(txt_grob) + 2*padding,
                              pos = row[i])

      plot <- gtable_add_grob(plot, txt_grob,
                              t = row[i] + 1,
                              b = row[i] + 1,
                              l = min(col),
                              r = max(col),
                              clip = "off",
                              name = nam_text)
    }

  }

  return(plot)

}
