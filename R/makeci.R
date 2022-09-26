
#' Create confidence interval grob
#'
#' @inheritParams forest
#' @param pch Numeric or character vector indicating what sort of plotting
#' symbol to use. See \code{\link[grid]{pointsGrob}}.
#' @param gp Graphical parameters.
#' @param t_height The height confidence interval line end vertices. If
#' value is `NULL` (default), no vertices will be drawn.
#' @param name name of the grob.
#' 
#' @return A gTree object
#'
#' @export
makeci <- function(est, lower, upper, pch, sizes = 1, gp = gpar(),
                   t_height = NULL, xlim = c(0, 1), nudge_y = 0,
                   name = NULL){

  gTree(
    est = est, lower = lower, upper = upper, pch = pch,
    size = sizes, gp = gp, t_height = t_height,
    xlim = xlim, nudge_y = nudge_y, name = name,
    # vp = viewport(xscale = xlim),
    cl = "makeci"
  )

}

#' @export
makeContext.makeci <- function(x) {
  tbvp <- viewport(xscale = x$xlim)
  if (is.null(x$vp))
    x$vp <- tbvp
  else
    x$vp <- vpStack(x$vp, tbvp)
  x
}

#' @export
makeContent.makeci <- function(x) {

  kids <- makeci_static(est = x$est, lower = x$lower, upper = x$upper,
                        pch = x$pch, size = x$size, gp =  x$gp,
                        t_height = x$t_height, xlim = x$xlim,
                        nudge_y = x$nudge_y)

  setChildren(x, kids)
}

# Main function for confidence interval
#' @keywords internal
makeci_static <- function(est, lower, upper, pch, size = 1, gp = gpar(),
                          t_height = NULL, xlim = c(0, 1), nudge_y = 0){

  # Return NULL if the CI is outside
  if(upper < min(xlim) | lower > max(xlim))
    return(gList(nullGrob()))

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
    vert <- nullGrob()
  }

  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    rec <- nullGrob()

  gList(rec, lng, vert)

}


# Create pooled summary diamond shape
make_summary <- function(est, lower, upper, sizes = 1, gp, xlim){

  # Return NULL if the CI is outside
  if(upper < min(xlim) | lower > max(xlim))
    return(NULL)

  polygonGrob(x = unit(c(lower, est, upper, est), "native"),
              y = unit(0.5 + c(0, 0.5 * sizes, 0, -0.5*sizes), "npc"),
              gp = gp,
              vp = viewport(xscale = xlim),
              name = "pooled.diamond")
}

