
#' Make arrow
#'
#' @param x0 Position of vertical line for 0 or 1.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param arrow_lab Label for the arrow, left and right.
#' @param is_exp If values is exponential.
#' @param col_width Width of the column arrow to be fitted.
#' @param arrow_gp Graphical parameters for arrow.
#'
#' @keywords internal
make_arrow <- function(x0 = 1, arrow_lab, arrow_gp, col_width, xlim, is_exp = FALSE){

  if(is_exp)
    x0 <- log(x0)

  gp <- arrow_gp$gp

  # Calculate width of left and right width
  left_col_w <- col_width * abs(x0 - xlim[1])/(abs(xlim[1]) + abs(xlim[2]))
  right_col_w <- col_width * abs(xlim[2] - x0)/(abs(xlim[1]) + abs(xlim[2]))

  txt_len <- sapply(arrow_lab, function(txt){
    tx_gb <- textGrob(txt, gp = gp)
    convertWidth(grobWidth(tx_gb), "char", valueOnly = TRUE)
  }, USE.NAMES = FALSE)


  # Left side
  if(arrow_gp$label_just == "start" | txt_len[1] > left_col_w){
    l_just <- "right"
    x_pos_l <- unit(x0, "native") - unit(0.05, "inches")
  }else{
    l_just <- "left"
    x_pos_l <- unit(0, "npc") + unit(0.05, "inches")
  }

  # Right side
  if(arrow_gp$label_just == "start" | txt_len[2] > right_col_w){
    r_just <- "left"
    x_pos_r <- unit(x0, "native") + unit(0.05, "inches")
  }else{
    r_just <- "right"
    x_pos_r <- unit(1, "npc") - unit(0.05, "inches")
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

  # Fill the column if the text is not wide enough
  if(txt_len[1] < left_col_w)
    t_cord_lft$xl <- unit(0, "npc")
  else
    t_cord_lft$xl <- x_pos_l - stringWidth(arrow_lab[1])

  if(txt_len[2] < right_col_w)
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
