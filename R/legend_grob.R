#' Create legends
#'
#' This function used to create legends for the forest plot.
#'
#' @param name Character string, Legend name.
#' @param label legend labels (expressions).
#' @param pch Legend symbol.
#' @param position Position of the legend, \code{"right"}, \code{"top"},
#' \code{"bottom"}.
#' @param hgap Horizontal gap between the legend entries,
#' see \code{\link[grid]{legendGrob}} for details.
#' @param vgap Vertical gap between the legend entries,
#' see \code{\link[grid]{legendGrob}} for details.
#' @param gp Graphical parameters.
#' @param ... Other parameters, not used currently.
#'
#' @return A frame grob
#'
#' @keywords internal
legend_grob <- function(name = "",
                        label,
                        position = c("right", "top", "bottom"),
                        hgap = unit(0.1, "lines"), #horizontal gap
                        vgap = unit(0.5, "lines"), #vertical gap
                        pch = 15,
                        gp = gpar(lty = 1,
                                  col = "black",
                                  fill = "black",
                                  fontsize = 12,
                                  fontfamily = ""),
                        ...
){

  position <- match.arg(position)

  # Legend title
  title_grob <- textGrob(label = name,
                         just = "left",
                         x = 0,
                         y = 0.5,
                         gp = gpar(fontsize = gp$fontsize,
                                   fontfamily = gp$fontfamily,
                                   fontface = 'bold',
                                   fill = 'black'))

  if(position %in% c("top", "bottom")){
    by_row <- FALSE
    ncol <- length(gp$col)

  }else{
    by_row <- TRUE
    ncol <- 1
  }

  # LegendGrob
  leg_grob <- legendGrob(label, pch = pch, ncol = ncol,
                         do.lines = TRUE, byrow = by_row,
                         hgap = hgap, vgap = vgap,
                         gp = gp)

  # Change legendGrob point color
  leg_grob <- edit_leg_point(leg_grob, gp$fill)

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

# Edit legendGrob point color
edit_leg_point <- function(leg, gp_col){
  # Find the point path
  lst <- grid.grep("point", leg, grep = TRUE, global = TRUE)
  # Extract name of the gPath
  g_paths <- sapply(lst, function(x){
    paste(sub(".*?::",'', x$path), x$name, sep = "::")
  })
  
  for(i in seq_along(g_paths)){
    leg <- editGrob(leg, 
                    gPath = g_paths[i],
                    gp = gpar(col = gp_col[i]))
  }
  return(leg)
}


