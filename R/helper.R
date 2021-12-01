#' Add vertical line
#'
#' @param x A numeric unit object specifying x-values.
#' @param gp An object of class \code{gpar}, see \link[grid::gpar].
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high)
#'
vert_line <- function(x, gp = grid::gpar(), xlim){
  linesGrob(x = unit(x, "native"),
            y = unit(c(0, 1), "npc"),
            gp = gp,
            vp = viewport(xscale = xlim))
}

