
#' Make arrow
#'
#' @param x0 Position of vertical line for 0 or 1.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param arrow_lab Label for the arrow, left and right.
#' @param is_exp If values is exponential.
#' @param arrow_gp Graphical parameters for arrow.
#'
#' @keywords internal
make_arrow <- function(x0 = 1, arrow_lab, arrow_gp, xlim, is_exp = FALSE){

  if(is_exp)
    x0 <- log(x0)

  check_width <- function(text, width){
    # txt_gb <- textGrob(text, gp = gp)
    convertWidth(stringWidth(text), "cm", valueOnly = TRUE) < abs(width)
  }

  gp <- arrow_gp$gp

  # Left side
  if(arrow_gp$label_just == "start" | !check_width(arrow_lab[1], xlim[1] - x0)){
    l_just <- "right"
    x_pos_l <- unit(x0, "native") - unit(0.05, "inches")
  }else{
    l_just <- "left"
    x_pos_l <- unit(xlim[1], "native") + unit(0.05, "inches")
  }

  # Right side
  if(arrow_gp$label_just == "start" | !check_width(arrow_lab[2], xlim[2] - x0)){
    r_just <- "left"
    x_pos_r <- unit(x0, "native") + unit(0.05, "inches")
  }else{
    r_just <- "right"
    x_pos_r <- unit(xlim[2], "native") - unit(0.05, "inches")
  }

  t_lft <- textGrob(arrow_lab[1],
                    x = x_pos_l,
                    y = unit(0.5, "npc"),
                    just = l_just,
                    gp = gp,
                    name="arrow.text.left")

  t_rgt <- textGrob(arrow_lab[2],
                    x = x_pos_r,
                    y = unit(0.5, "npc"),
                    just = r_just,
                    gp = gp,
                    name="arrow.text.right")

  t_cord_lft <- getCorners(t_lft)
  t_cord_rgt <- getCorners(t_rgt)

  # Full length if the text is smaller than x-axis
  if(check_width(arrow_lab[1], xlim[1] - x0))
    t_cord_lft$xl <- unit(0, "npc")
  else
    t_cord_lft$xl <- x_pos_l - stringWidth(arrow_lab[1])

  if(check_width(arrow_lab[2], xlim[2] - x0))
    t_cord_rgt$xr <- unit(1, "npc")
  else
    t_cord_rgt$xr <- x_pos_r + stringWidth(arrow_lab[2])

  s_lft <- segmentsGrob(x0 = t_cord_lft$xl,
                        y0 = t_cord_lft$yt + unit(.1, "lines") + arrow_gp$length,
                        x1 = unit(x0, "native") - unit(0.05, "inches"),
                        y1 = t_cord_lft$yt + unit(.1, "lines") + arrow_gp$length,
                        gp = gp,
                        arrow = arrow(length = arrow_gp$length,
                                      ends = "first",
                                      type = arrow_gp$type),
                        name="arrow.left")

  s_rgt <- segmentsGrob(x0 = t_cord_rgt$xr,
                        y0 = t_cord_rgt$yt + unit(.1, "lines") + arrow_gp$length,
                        x1 = unit(x0, "native") + unit(0.05, "inches"),
                        y1 = t_cord_rgt$yt + unit(.1, "lines") + arrow_gp$length,
                        gp = gp,
                        arrow = arrow(length = arrow_gp$length,
                                      ends = "first",
                                      type = arrow_gp$type),
                        name="arrow.right")

  grobTree(gList(t_lft, s_lft, t_rgt, s_rgt),
           vp = viewport(xscale = xlim),
           name = "arrow")

}
