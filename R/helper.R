# Add vertical line
vert_line <- function(x, gp = grid::gpar(), xlim, is_exp = FALSE){
  
  if(is_exp)
    x <- log(x)
    
  segmentsGrob(x0 = unit(x,"native"),
               x1 = unit(x,"native"),
               y0 = unit(0.01,"npc"),
               y1 = unit(.99,"npc"),
               gp = gp,
               vp = viewport(xscale = xlim))
}


# Get grob corners
getCorners <- function(x) {
    list(xl=grobX(x, 180), xr=grobX(x, 0),
         yb=grobY(x, 270), yt=grobY(x, 90))
  }

# Draw summary CI
summary_ci <- function(est, lower, upper, size = 1, gp = gpar(), xlim){
  polygonGrob(
    x = c(lower, est, upper, est),
    y = unit(0.5 + c(0, 0.5 * size, 0, -0.5 * size), "npc"),
    gp = gp,
    name = "summary.ci",
    vp = viewport(xscale = xlim))
}

# Cehck if same length
same_len <- function(...){
  lst <- list(...)
  len <- vapply(lst, length, FUN.VALUE = 1L)
  length(unique(len)) == 1
}

