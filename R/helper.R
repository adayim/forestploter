#' Add vertical line
#'
#' @param x A numeric unit object specifying x-values.
#' @param gp An object of class \code{gpar}, see \code{\link[grid]{gpar}}.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#'
vert_line <- function(x, gp = grid::gpar(), xlim){
  segmentsGrob(x0 = unit(x,"native"),
               x1 = unit(x,"native"),
               y0 = unit(0.01,"npc"),
               y1 = unit(.99,"npc"),
               gp = gp,
               vp = viewport(xscale = xlim))
}

# Default theme
theme_default <- gridExtra::ttheme_minimal(
  core=list(
    fg_params = list(hjust = 0, x = 0.05),   # font
    bg_params = list(fill=c(rep(c("#eff3f2", "white"),
                                length.out=4))), # bands
    padding = unit(c(4, 2.5), "mm")
  ),
  colhead = list(
    fg_params = list(hjust = 0, x = 0.05, #parse=TRUE,
                     fontface=2L),
    bg_params = list(fill = "white"),
    padding = unit(c(4, 4), "mm")
  )
)

# Get grob corners
getCorners <- function(x) {
    list(xl=grobX(x, 180), xr=grobX(x, 0),
         yb=grobY(x, 270), yt=grobY(x, 90))
  }

