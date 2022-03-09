
#' Create confidence interval
#'
#' @param est Point estimates, numeric
#' @param lower Lower bound
#' @param upper Upper bound
#' @param size Size of the point
#' @param pch Numeric or character vector indicating what sort of plotting
#' symbol to use. See \code{\link[grid]{pointsGrob}}.
#' @param gp Grphical parameters.
#' @param t_height The height confidence interval line end vertices. If 
#' value is `NULL` (default), no vertices will be drawn.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param nudge_y Offset Y coordinates.
#' @param color Color of the point and the line
#'
#' @keywords internal
makeci <- function(est, lower, upper, pch, size = 1, gp = gpar(), 
                   t_height = NULL, xlim = c(0, 1), nudge_y = 0){

  rec <- pointsGrob(x = unit(est, "native"),
                    y = 0.5 + nudge_y,
                    pch = pch,
                    size = unit(size, "char"),
                    gp = gp)

  if(upper > max(xlim) | lower < min(xlim)){
    # Both side arrow
    if(upper > max(xlim) & lower < min(xlim)){
      x_pos <- unit(c(0, 1), c("npc", "npc"))
      arrow_side <- "both"
      x_vert <- NULL
    }

    # Left side arrow
    else if(lower < min(xlim) & upper < max(xlim)){
      x_pos <- unit(c(0, upper), c("npc", "native"))
      arrow_side <- "first"
      x_vert <- unit(upper, "native")
    }

    # Right side arrow
    else{
      x_pos <- unit(c(lower, 1), c("native", "npc"))
      arrow_side <- "last"
      x_vert <- unit(lower, "native")
    }

    lng <- linesGrob(x=x_pos, y = 0.5 + nudge_y,
                     arrow=arrow(length=unit(0.05, "inches"),
                                 ends = arrow_side),
                     gp=gp)
  } else {
    lng <- linesGrob(x=unit(c(lower, upper), "native"), y=0.5 + nudge_y,
                     gp=gp)
    
    x_vert <- unit(c(lower, upper),  "native")
  }

  # Draw T end to the CI
  if(!is.null(t_height) & !is.null(x_vert)){
    if(!is.unit(t_height))
      t_height <- unit(t_height, "npc")

    vert <- segmentsGrob(x0 = x_vert, y0 = unit(0.5 + nudge_y, "npc") - t_height/2,
                         x1 = x_vert, y1 = unit(0.5 + nudge_y, "npc") + t_height/2,
                         gp = gp,
                         name = "T.end")
  }else {
    vert <- NULL
  }
    

  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    grobTree(gList(lng, vert),
             vp = viewport(xscale = xlim),
             name = "ci")
  else
    grobTree(gList(rec, lng, vert),
             vp = viewport(xscale = xlim),
             name = "ci")

}

# Create pooled summary diamond shape
make_summary <- function(est, lower, upper, size = 1, gp, xlim){
  polygonGrob(x = unit(c(lower, est, upper, est), "native"),
              y = unit(0.5 + c(0, 0.5 * size, 0, -0.5*size), "npc"),
              gp = gp,
              vp = viewport(xscale = xlim),
              name = "pooled.diamond")
}


