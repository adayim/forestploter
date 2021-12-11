#' Create legends
#'
#' This function used to create legends for the forest plot.
#'
#' @param name Character string, Legend name.
#' @param label legend labels (expressions).
#' @param color Colors for the group
#' @param pch Legend symbol.
#' @param position Position of the legend, \code{"right"}, \code{"top"},
#' \code{"bottom"}.
#' @param hgap Horizontal gap between the legend entries,
#' see \code{\link[grid]{legendGrob}} for details.
#' @param vgap Vertical gap between the legend entries,
#' see \code{\link[grid]{legendGrob}} for details.
#' @param fontsize Font size of the legend.
#' @param fontfamily Font family of the legend.
#' @param ... Other parameters, not used currently.
#'
#' @return A frame grob
#'
#'
legend_grob <- function(name = "",
                        label,
                        color,
                        position = c("right", "top", "bottom"),
                        hgap = unit(0.1, "lines"), #horizontal gap
                        vgap = unit(0.5, "lines"), #vertical gap
                        pch = 15,
                        fontsize = 12,
                        fontfamily = "",
                        ...
){

  position <- match.arg(position)

  # Legend title
  title_grob <- textGrob(label = name,
                         just = "left",
                         x = 0,
                         y = 0.5,
                         gp = gpar(fontsize = fontsize,
                                   fontfamily = fontfamily,
                                   fontface = 'bold',
                                   fill = 'black'))

  if(position %in% c("top", "bottom")){
    by_row <- FALSE
    ncol <- length(color)

  }else{
    by_row <- TRUE
    ncol <- 1
  }

  leg_grob <- legendGrob(label, pch = pch, ncol = ncol,
                         do.lines = TRUE, byrow = by_row,
                         hgap = hgap, vgap = vgap,
                         gp = gpar(col = color,
                                   fill = color,
                                   fontsize = fontsize,
                                   fontfamily = fontfamily))

  u0 <- unit(0, "npc")
  u1 <- unit(0.02, "npc")

  if(position %in% c("top", "bottom")){

    packGrob(frame = packGrob(frameGrob(name = "legend"), title_grob,
                              border = unit.c(u0, u1, u0, u0)),
             grob = leg_grob,
             side = "right")

  }else{
    packGrob(frame = packGrob(frameGrob(name = "legend"), title_grob,
                              border = unit.c(u0, u0, u1, u0)),
             grob = leg_grob,
             side = "bottom")
  }
}



#' CI settings for group
#'
#' This is to set the point group name, symbol and color of the CI.
#'
#' @param name Title of the legend.
#' @param position Position of the legend, \code{"right"}, \code{"top"},
#' \code{"bottom"}.
#' @param value Legend labels (expressions)
#' @param pch Shape of the point symbol, see \code{\link[grid]{pointsGrob}}.
#' @param color Colors for the group/confidence interval.
#'
#' @export
#'
set_legend <- function(name = "", position = NULL, value = NULL, pch = 15, color){

  max_len <- list(value, pch, color)
  max_len <- max(vapply(max_len, length, FUN.VALUE = 1L), na.rm = TRUE)

  if(is.null(value))
    value <- ""

  value <- rep_len(value, max_len)
  pch <- rep_len(pch, max_len)
  color <- rep_len(color, max_len)

  if(is.null(position))
    position <- "right"

  position <- match.arg(position, c("right", "top", "bottom"))

  out <- list(name = name,
              position = position,
              label = value,
              pch = pch,
              color = color)

  class(out) <- union("ciset", out)

  return(out)
}

# Generate default grouping
.gen_legend <- function(group_number){

  col_set <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
               "#ffff33","#a65628","#f781bf","#999999")

  list(name = "Group",
       position = "right",
       value = paste("Group", 1:group_number),
       pch = 0:(group_number - 1),
       color = col_set[1:group_number])
}
