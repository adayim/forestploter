
#' Create horizontal boxplot grob
#'
#' @inheritParams forest
#' @param est Median value.
#' @param lower Lower whisker.
#' @param upper Upper whisker.
#' @param lowhinge Lower hinge, a standard whisker will be drawn if this
#' is missing.
#' @param uphinge Ppper hinge, a standard whisker will be drawn if this
#' is missing.
#' @param hinge_height Height of the hinge, default is 0.2.
#' @param pch Numeric or character vector indicating what sort of plotting
#' symbol to use. See \code{\link[grid]{pointsGrob}}.
#' @param gp Graphical parameters of \code{\link[grid]{gpar}}. Please refer
#'  to \code{\link{forest_theme}} for more details.
#' @param gp_box Graphical parameters passed to the hinge, this will be
#' passed to \code{\link[grid]{rectGrob}}. This does not support multiple groups.
#' @param t_height Height of the whisker end vertices. If value is `NULL` (default),
#'  no vertices will be drawn.
#' @param xlim Limits for the x axis as a vector of length 2, i.e. c(low, high).
#' @param nudge_y Horizontal adjustment to nudge groups by, must be within 0 to 1.
#'
#' @return A gTree object
#' @seealso \code{\link[grid]{pointsGrob}} \code{\link[grid]{gpar}} \code{\link[grid]{rectGrob}}
#' \code{\link[grid]{linesGrob}} \code{\link[grid]{segmentsGrob}}
#' @example inst/examples/boxplot-example.R
#'
#' @export
make_boxplot <- function(est, lower, upper, lowhinge, uphinge,
                         hinge_height = 0.2,
                         pch, sizes = 1, gp = gpar(),
                         gp_box = gp,
                         t_height = NULL, xlim = c(0, 1), nudge_y = 0){

  gTree(
    est = est, lower = lower, upper = upper, pch = pch,
    lowhinge = lowhinge, uphinge = uphinge, hinge_height = hinge_height,
    size = sizes, gp = gp, t_height = t_height, gp_box = gp_box,
    xlim = xlim, nudge_y = nudge_y, name = NULL,
    cl = "make_boxplot"
  )

}

#' @export
makeContext.make_boxplot <- function(x) {
  tbvp <- viewport(xscale = x$xlim, clip = "on")
  if (is.null(x$vp))
    x$vp <- tbvp
  else
    x$vp <- vpStack(x$vp, tbvp)
  x
}

#' @export
makeContent.make_boxplot <- function(x) {

  kids <- boxplot_static(est = x$est, lower = x$lower, upper = x$upper,
                          pch = x$pch, size = x$size, gp =  x$gp,
                          lowhinge = x$lowhinge, uphinge = x$uphinge,
                          hinge_height = x$hinge_height, gp_box = x$gp_box,
                          t_height = x$t_height, xlim = x$xlim,
                          nudge_y = x$nudge_y)

  setChildren(x, kids)
}

# Main function for confidence interval
#' @keywords internal
boxplot_static <- function(est, lower, upper, pch,
                          lowhinge, uphinge, hinge_height,
                          size = 1, gp = gpar(), gp_box = gpar(),
                          t_height = NULL, xlim = c(0, 1), nudge_y = 0){

  # Return NULL if the CI is outside
  if(upper < min(xlim) | lower > max(xlim))
    return(gList(nullGrob()))

  # Point estimation
  rec_gp <- gp
  rec_gp$col <- gp$fill
  med_rec <- pointsGrob(x = unit(est, "native"),
                        y = 0.5 + nudge_y,
                        pch = pch,
                        size = unit(size, "char"),
                        gp = rec_gp,
                        name = "point")

  # Plot whisker
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
                     gp=gp, name = "whisker")
  } else {
    lng <- linesGrob(x=unit(c(lower, upper), "native"), y=0.5 + nudge_y,
                     gp=gp, name = "whisker")

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

  # Plot hinge
  if((is.na(uphinge) | is.na(lowhinge)) | (lowhinge > max(xlim) | uphinge < min(xlim))){
    hinge_rect <- nullGrob()
  }else{
    hinge_width <- uphinge - lowhinge

    hinge_rect <- rectGrob(x = unit(lowhinge, "native"),
                           y = unit(0.5 + nudge_y, "npc"),
                           just = "left",
                           width = unit(hinge_width, "native"),
                           height = unit(hinge_height, "char"),
                           gp = gp_box,
                           name = "hinge")
  }


  # No dots if outside
  if(est > max(xlim) | est < min(xlim))
    med_rec <- nullGrob()

  gList(lng, hinge_rect, vert, med_rec)

}

