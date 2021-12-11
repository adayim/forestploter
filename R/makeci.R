
#' Create confidence interval
#'
#' @param est Point estimates, numeric
#' @param lower Lower bound
#' @param upper Upper bound
#' @param size Size of the point
#' @param pch Numeric or character vector indicating what sort of plotting
#' symbol to use. See \code{\link[grid]{pointsGrob}}.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param nudge_y Offset Y coordinates.
#' @param color Color of the point and the line
#'
#'
makeci <- function(est, lower, upper, pch, size = 1, xlim = c(0, 1), nudge_y = 0, color = "black"){

  rec <- pointsGrob(x = unit(est, "native"),
                    y = 0.5 + nudge_y,
                    pch = pch,
                    size = unit(size, "char"),
                    gp = gpar(col= color))

  if(upper > max(xlim) | lower < min(xlim)){
    # Both side arrow
    if(upper > max(xlim) & lower < min(xlim)){
      x_pos <- unit(c(0, 1), c("npc", "npc"))
      arrow_side <- "both"
    }

    # Left side arrow
    else if(lower < min(xlim) & upper < max(xlim)){
      x_pos <- unit(c(0, upper), c("npc", "native"))
      arrow_side <- "first"
    }

    # Right side arrow
    else{
      x_pos <- unit(c(lower, 1), c("native", "npc"))
      arrow_side <- "last"
    }

    lng <- linesGrob(x=x_pos, y = 0.5 + nudge_y,
                     arrow=arrow(length=unit(0.05, "inches"),
                                 ends = arrow_side),
                     gp=gpar(col= color))
  } else {
    lng <- linesGrob(x=unit(c(lower, upper), "native"), y=0.5 + nudge_y,
                     gp=gpar(col= color))
  }

  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    grobTree(gList(lng),
             vp = viewport(xscale = xlim),
             name = "ci")
  else
    grobTree(gList(rec, lng),
             vp = viewport(xscale = xlim),
             name = "ci")

}

