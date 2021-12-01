
#' Create confidence interval
#'
#' @param est Point estimates, numeric
#' @param lower Lower bound
#' @param upper Upper bound
#' @param size Size of the point
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#'
#' @importFrom grid pointsGrob gpar unit linesGrob gList grobTree viewport
#'
makeci <- function(est, lower, upper, size = 1, xlim = c(0, 1)){

  rec <- pointsGrob(x = unit(est, "native"),
                    y = 0.5,
                    pch = 15,
                    size = unit(size, "char"),
                    gp = gpar(fill="black"))

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

    lng <- linesGrob(x=x_pos, y=.5,
                     arrow=arrow(length=unit(0.05, "inches"),
                                 ends = arrow_side))
  } else {
    lng <- linesGrob(x=unit(c(lower, upper), "native"), y=0.5,
                     gp=gpar(col="black"))
  }

  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    grobTree(gList(lng),
             vp = viewport(xscale = xlim))
  else
    grobTree(gList(rec, lng),
             vp = viewport(xscale = xlim))

}

