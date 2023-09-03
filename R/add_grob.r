
#' Add grob in cells
#'
#' Draw grobs in any cells.
#'
#' @param plot A forest plot object.
#' @param row A numeric value or vector indicating row(s) to draw a grob.
#' @param col A numeric value or vector indicating the columns to draw a grob.
#' @param part The border will be added to \code{"body"} (default) or
#' \code{"header"}.
#' @param order Order in which the grobs should be plotted. Use \code{'top'} 
#' (default) to draw the grob above everything, \code{'text'} on the top of text 
#' given by plot data but below everything else, \code{'background'} plot on the 
#' top of background but below everything else, \code{'bottom'} below everything.
#' @param gb_fn Grob function
#' @param ... Other parameters to be passed to \code{gb_fn}.
#'
#' @return A \code{\link[gtable]{gtable}} object.
#' @seealso \code{\link[gtable]{gtable_add_grob}}
#' @rdname add_grob
#' @export
add_grob <- function(plot,
                     row = NULL,
                     col = NULL,
                     part = c("body", "header"),
                     order = c("top", "text", "background", "bottom"),
                     gb_fn,
                     ...){
  
  dots <- list(...)
  arg <- match(names(formals(gb_fn)), names(dots))
  dots[arg[!is.na(arg)]]
  
  if(length(row) != 1 & !all(diff(row) == 1))
    stop("row must be scalar value or a consecutive vector.")
  
  if(length(col) != 1 & !all(diff(col) == 1))
    stop("col must be scalar value or a consecutive vector.")

  if(!inherits(plot, "forestplot"))
    stop("plot must be a forestplot object.")


  part <- match.arg(part)
  order <- match.arg(order)

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

  custom_grob <- do.call(gb_fn, dots)

  if(length(row) == 1)
    row_name <- row
  else
    row_name <- paste0(min(row), ":", max(row))

  if(length(col) == 1)
    col_name <- col
  else
    col_name <- paste0(min(col), ":", max(col))

  # Get the order of the grob
  if(order == "top")
    z <- Inf
  if(order == "bottom")
    z <- -Inf
  if(order == "text"){
    if(part == "header")
      z <- max(l$z[which(l$name == "colhead-fg")])
    else
      z <- max(l$z[which(l$name == "core-fg")])
  }

  if(order == "background"){
    if(part == "header")
      z <- max(l$z[which(l$name == "colhead-bg")])
    else
      z <- max(l$z[which(l$name == "core-bg")])
  }

  plot <- gtable_add_grob(plot, custom_grob,
                          t = min(row),
                          b = max(row),
                          l = min(col),
                          r = max(col),
                          z = z,
                          clip = "off",
                          name = paste("custom.grob", row_name, col_name, sep = "-"))

  return(plot)

}

