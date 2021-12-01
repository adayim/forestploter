
#' Make arrow
#'
#' @param x0 Position of vertical line for 0 or 1.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param arrow.lab Label for the arrow, left and right.
#' @param gp Graphical parameters for arrow.
#'

make_arrow <- function(x0 = 1, arrow.lab, gp, xlim){

  rt <- arrow_text(arrow.lab[1], gp = gp, x = unit(x0, "native") + unit(0.05, "inches"), direction = "right")
  lt <- arrow_text(arrow.lab[2], gp = gp, x = unit(x0, "native") - unit(0.05, "inches"), direction = "left")

  grobTree(gList(rt, lt), vp = viewport(xscale = xlim, height = max(grobHeight(rt), grobHeight(lt))))

}


arrow_text <- function(label, gp, x, direction) {

  if(direction == "left"){
    just <- "right"
    ends <- "first"
  }else{
    just <- "left"
    ends <- "last"
  }

  t <- textGrob(label, x = x, y = unit(0.4, "npc"), just = just,
                gp = gp, 
                name="arrow.text")
  t_cord <- getCorners(t)
  s <- segmentsGrob(t_cord$xl,
                    t_cord$yt + unit(.2, "lines"),
                    t_cord$xr,
                    t_cord$yt + unit(.2, "lines"),
                    gp = gp, 
                    arrow = arrow(length=unit(0.05, "inches"),
                                  ends = ends),
                    name="arrow")

  gTree(children=gList(t, s))
}


