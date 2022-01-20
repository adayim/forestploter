
#' Make arrow
#'
#' @param x0 Position of vertical line for 0 or 1.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param arrow_lab Label for the arrow, left and right.
#' @param gp Graphical parameters for arrow.
#'
#' @keywords internal
make_arrow <- function(x0 = 1, arrow_lab, gp, xlim){

  t_lft <- textGrob(arrow_lab[1],
                    x = unit(x0, "native") - unit(0.05, "inches"),
                    y = unit(0.5, "npc"), just = "right",
                    gp = gp,
                    name="arrow.text.left")

  t_rgt <- textGrob(arrow_lab[2],
                    x = unit(x0, "native") + unit(0.05, "inches"),
                    y = unit(0.5, "npc"), just = "left",
                    gp = gp,
                    name="arrow.text.right")

  t_cord_lft <- getCorners(t_lft)
  t_cord_rgt <- getCorners(t_rgt)

  s_lft <- segmentsGrob(t_cord_lft$xl,
                        t_cord_lft$yt + unit(.2, "lines"),
                        t_cord_lft$xr,
                        t_cord_lft$yt + unit(.2, "lines"),
                        gp = gp,
                        arrow = arrow(length=unit(0.05, "inches"),
                                      ends = "first"),
                        name="arrow.left")

  s_rgt <- segmentsGrob(t_cord_rgt$xl,
                        t_cord_rgt$yt + unit(.2, "lines"),
                        t_cord_rgt$xr,
                        t_cord_rgt$yt + unit(.2, "lines"),
                        gp = gp,
                        arrow = arrow(length=unit(0.05, "inches"),
                                      ends = "last"),
                        name="arrow.right")

  grobTree(gList(t_lft, s_lft, t_rgt, s_rgt),
           vp = viewport(xscale = xlim),
           name = "arrow")

}
