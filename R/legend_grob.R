#' Create legends
#'
#' This function used to create legends for the forest plot.
#'
#' @param name Character string, Legend name.
#' @param color Colors for the group
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
#' @export
#'
#'
legend_grob <- function(name = "",
                        color,
                        position = c("right", "top", "bottom"),
                        hgap = unit(0.1, "lines"), #horizontal gap
                        vgap = unit(0.5, "lines"), #vertical gap
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

  # Legend
  leg_labs <- if(is.null(names(color))) rep("", length(color)) else names(color)

  if(position %in% c("top", "bottom")){
    by_row <- FALSE
    ncol <- length(color)

  }else{
    by_row <- TRUE
    ncol <- 1
  }

  leg_grob <- legendGrob(leg_labs, pch=15, ncol = ncol,
                         do.lines = TRUE, byrow = by_row,
                         hgap = hgap, vgap = vgap,
                         gp = gpar(col = color,
                                   fill = color,
                                   fontsize = fontsize,
                                   fontfamily = fontfamily))

  u0 <- unit(0, "npc")
  u1 <- unit(0.02, "npc")

  if(position %in% c("top", "bottom")){

    packGrob(frame = packGrob(frameGrob(), title_grob,
                              border = unit.c(u0, u1, u0, u0)),
             grob = leg_grob,
             side = "right")

  }else{
    packGrob(frame = packGrob(frameGrob(), title_grob,
                              border = unit.c(u0, u0, u1, u0)),
             grob = leg_grob,
             side = "bottom")
  }
}
