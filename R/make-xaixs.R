#' Create xaxis
#'
#' This function used to xais for the forest plot.
#'
#' @param at Numerical vector, create ticks at given values.
#' @param xlab X-axis label.
#' @param is_exp If values is exponential.
#' @param gp Graphical parameters for arrow.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#'
#' @return A grob
#'
#' @keywords internal
make_xaxis <- function(at, xlab = NULL, is_exp = FALSE, gp = gpar(), xlim){

  if(is_exp){
    label_at <- log(round(exp(at), 1))
    labels <- as.character(round(exp(at), 1))
  }else {
    label_at <- round(at, 1)
    labels <- as.character(round(at, 1))
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
                  check.overlap=TRUE,
                  name = "label")

  if(!is.null(xlab)){
    xlab_gb <- textGrob(xlab,
                        x = unit(ifelse(is_exp, 1, 0), "native"),
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
