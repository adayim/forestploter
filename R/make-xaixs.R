#' Create x-axis
#'
#' This function used to x-axis for the forest plot.
#'
#' @inheritParams forest
#' @param at Numerical vector, create ticks at given values.
#' @param x0 Position of vertical line for 0 or 1.
#' @param gp Graphical parameters for arrow.
#' @param xlab_gp Graphical parameters for xlab.
#'
#' @return A grob
#'
#' @keywords internal
make_xaxis <- function(at,
                       xlab = NULL,
                       x0 = 1,
                       x_trans = "none",
                       ticks_digits = 1,
                       gp = gpar(),
                       xlab_gp = NULL,
                       xlim){

  labels <- trimws(xscale(at, scale = x_trans, type = "format", format_digits = ticks_digits))
  x0 <- xscale(x0, scale = x_trans, type = "scale")
  label_at <- xscale(as.numeric(labels), scale = x_trans, type = "scale")

  maj <- linesGrob(x = unit(c(min(xlim), max(xlim)), "native"),
                   y = unit(c(0.99, 0.99), "npc"),
                   gp = gp,
                   name="major")

  maj_cord <- getCorners(maj)

  tick <- segmentsGrob(x0 = unit(label_at, "native"), y0 = maj_cord$yb,
                       x1 = unit(label_at, "native"), y1 = maj_cord$yb - unit(.5, "lines"),
                       gp = gp,
                       name = "tick")

  lab <- textGrob(labels,
                  x = unit(label_at, "native"),
                  y = maj_cord$yb - unit(1, "lines"),
                  gp = gp,
                  # check.overlap=TRUE,
                  name = "label")

  if(!is.null(xlab)){
    if(xlab_gp$just != "refline"){
      x_pos <- unit(0.5, "npc")
    }else{
      x_pos <- unit(x0, "native")
    }
    xlab_gb <- textGrob(xlab,
                        x = x_pos,
                        y = maj_cord$yb - unit(2, "lines"),
                        gp = xlab_gp$gp,
                        check.overlap=TRUE,
                        name = "xlab")

    grobTree(gList(maj, tick, lab, xlab_gb),
             vp = viewport(xscale = xlim),
             name = "xaxis")

  }else{
    grobTree(gList(maj, tick, lab),
             vp = viewport(xscale = xlim),
             name = "xaxis")
  }


}
