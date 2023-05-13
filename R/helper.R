#' Get widths and height the forestplot
#'
#' \code{get_wh} can be used to find the correct width and height of the forestplot 
#' for saving, as the width and height is difficult to fit for saving.
#'
#' @param plot A forest plot object.
#' @param unit Unit of the plot size in `units` ("in", "cm", or "mm")to be saved.
#'
#' @return A named vector of width and height
#' @export
#' @examples
#' \dontrun{
#'  dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
#'  dt <- dt[1:6,1:6]
#'
#'  dt$` ` <- paste(rep(" ", 20), collapse = " ")
#'
#'  p <- forest(dt[,c(1:3, 7)],
#'              est = dt$est,
#'              lower = dt$low,
#'              upper = dt$hi,
#'              ci_column = 4)
#'
#' # get_wh example
#' p_wh <- get_wh(p)
#' pdf('test.pdf',width = p_wh[1], height = p_wh[2])
#' plot(p)
#' dev.off()
#' }

get_wh <- function(plot, unit = c("in", "cm", "mm")){

  if(!inherits(plot, "forestplot"))
    stop("plot is not a forestplot")

  unit <- match.arg(unit)
  h <- convertHeight(sum(plot$heights), unit, TRUE)
  w <- convertWidth(sum(plot$widths), unit, TRUE)
  return(c(width = w, height = h))
}


# Add vertical line
vert_line <- function(x, gp = grid::gpar(), xlim, x_trans = "none"){

  if(x_trans != "none")
    x <- xscale(x, scale = x_trans)

  out_indx <- x > max(xlim) | x < min(xlim)
  if(all(out_indx)){
    return(nullGrob())
  }else{
    segmentsGrob(x0 = unit(x[!out_indx],"native"),
                x1 = unit(x[!out_indx],"native"),
                y0 = unit(0.01,"npc"),
                y1 = unit(.99,"npc"),
                gp = gp,
                vp = viewport(xscale = xlim))
  }
    
}


# Get grob corners
getCorners <- function(x) {
    list(xl=grobX(x, 180), xr=grobX(x, 0),
         yb=grobY(x, 270), yt=grobY(x, 90))
  }


# Count leading zeros between the decimal point and first nonzero digit
count_zeros <- function(x) {   
  # ref: https://stackoverflow.com/a/35559346/5714545
  lead0 <- -log10(abs(x) - floor(abs(x)))
  lead0 <- floor(lead0) - (lead0 %% 1 < .Machine$double.eps ^ 0.5) 
  lead0[is.na(lead0)] <- 0
  if(all(x > 0))
    0
  else
    max(lead0) + 1
}

# Count decimal places
count_decimal <- function(x) {
  ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
         nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
         0)
}
