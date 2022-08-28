#' Create x-axis
#'
#' This function used to x-axis for the forest plot.
#'
#' @inheritParams forest
#' @param at Numerical vector, create ticks at given values.
#' @param x0 Position of vertical line for 0 or 1.
#' @param gp Graphical parameters for arrow.
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
                       xlim){

  if(x_trans != "none"){
    label_at <- xscale(round(xscale(at, scale = x_trans, type = "inv"), ticks_digits), scale = x_trans)
    x0 <- xscale(x0, scale = x_trans)
    labels <- xscale(at, scale = x_trans, type = "format", format_digits = ticks_digits)
  }else {
    label_at <- round(at, ticks_digits)
    labels <- trimws(formatC(at, format="f", digits = ticks_digits, drop0trailing = is.integer(ticks_digits)))
  }

  maj <- linesGrob(x = unit(c(min(xlim), max(xlim)), "native"),
                   y = unit(c(0.99, 0.99), "npc"),
                   gp = gp,
                   name="major")

  maj_cord <- getCorners(maj)

  tick <- segmentsGrob(x0 = unit(at, "native"), y0 = maj_cord$yb,
                       x1 = unit(at, "native"), y1 = maj_cord$yb - unit(.5, "lines"),
                       gp = gp,
                       name = "tick")

  lab <- textGrob(labels,
                  x = unit(label_at, "native"),
                  y = maj_cord$yb - unit(1, "lines"),
                  gp = gp,
                  # check.overlap=TRUE,
                  name = "label")

  if(!is.null(xlab)){
    xlab_gb <- textGrob(xlab,
                        x = unit(x0, "native"),
                        y = maj_cord$yb - unit(2, "lines"),
                        gp = gp,
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
