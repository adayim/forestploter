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

# Cehck if same length
same_len <- function(...){
  lst <- list(...)
  len <- vapply(lst, length, FUN.VALUE = 1L)
  length(unique(len)) == 1
}

