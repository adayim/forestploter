
#' Make x-axis and arrow
#'
#' @param at A numeric vector of x-value locations for the tick marks.
#' @param x0 Position of vertical line for 0 or 1.
#' @param labels Labels of the tick
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param arrow.lab Label for the arrow, left and right.
#'
#' @importFrom grid segmentsGrob textGrob grobHeight unit.c arrow
makexaxis <- function(at, x0 = 1, labels = NULL, xlim, arrow.lab = NULL){

  if(is.null(labels))
    labels <- as.character(at)

  maj <- linesGrob(unit(c(min(at), max(at)), "native"),
                   unit(c(1, 1), "npc"), name="major")

  tick <- segmentsGrob(unit(at, "native"), unit(1, "npc"),
                       unit(at, "native"), unit(1, "npc") - unit(.5, "lines"))

  lab <- textGrob(labels, unit(at, "native"),
                  unit(1, "npc") - unit(1, "lines"),
                  just="centre", rot=0, check.overlap=TRUE)

  if(!is.null(arrow.lab)){

    y.0 <- unit(1, "npc") - unit(1.2, "lines") - grobHeight(lab)

    # Left arrow
    ln_left <- linesGrob(x = unit.c(unit(x0, "native") - unit(0.05, "npc"),
                                    unit(min(xlim), "native")), y= y.0,
                         arrow=arrow(length=unit(0.05, "inches")))

    # Right arrow
    ln_right <- linesGrob(x = unit.c(unit(x0, "native") + unit(0.05, "npc"),
                                     unit(max(xlim), "native")), y = y.0,
                          arrow=arrow(length=unit(0.05, "inches")))
    # Left text
    lab_left <- textGrob(arrow.lab[1], unit(x0, "native") - unit(0.05, "npc"),
                         y.0 - unit(.5, "lines"), just="right")

    # Right text
    lab_right <- textGrob(arrow.lab[2], unit(x0, "native") + unit(0.05, "npc"),
                          y.0 - unit(.5, "lines"), just="left")

    grobTree(gList(maj, tick, lab, ln_left, ln_right, lab_left, lab_right),
             vp = viewport(xscale = xlim))
  }else{
    grobTree(gList(maj, tick, lab),
             vp = viewport(xscale = xlim))
  }
}

