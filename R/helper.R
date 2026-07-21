#' Get width and height of the forestplot
#'
#' \code{get_wh} can be used to find the correct width and height of the
#' forestplot for saving, as the width and height are difficult to estimate
#' otherwise.
#'
#' @param plot A forest plot object.
#' @param unit Unit for the returned width and height. One of \code{"in"},
#' \code{"cm"}, or \code{"mm"}.
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


# Add vertical line. Named with the `make_*` prefix to avoid shadowing the
# `vert_line` parameter of `forest()`.
make_vert_line <- function(x, gp = grid::gpar(), xlim, x_trans = "none", nrow = 10){

  if(x_trans != "none")
    x <- xscale(x, scale = x_trans)

  # Multiplier
  denom <- max(c(nrow, 10))

  out_indx <- x > max(xlim) | x < min(xlim)
  if(all(out_indx)){
    return(nullGrob())
  }else{
    segmentsGrob(x0 = unit(x[!out_indx],"native"),
                x1 = unit(x[!out_indx],"native"),
                y0 = unit(0,"npc") + unit(0.1,"npc")/denom,
                y1 = unit(1,"npc") - unit(0.1,"npc")/denom,
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
  if(all(x %% 1 == 0, na.rm = TRUE))
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


# Resolve `sizes` to an absolute height.
#
# `sizes` is a multiple of one line of text, so 1 is `base_size` tall. This is
# the same quantity `unit(size, "char")` would give, but resolved from the gp we
# were handed rather than from the current viewport: the confidence interval
# grobs are added to the table with `gtable_add_grob`, and neither the gtable nor
# the enclosing cell sets a `fontsize`, so a "char" unit would silently fall back
# to the pointsize the device happened to be opened with instead of the theme's
# `base_size`.
size_bigpts <- function(size, gp = gpar()) {
  fs <- if (is.null(gp$fontsize)) 12 else gp$fontsize
  cx <- if (is.null(gp$cex)) 1 else gp$cex
  unit(size * fs * cx, "bigpts")
}


# Vertical cell padding of the table body, in big points.
#
# Only used to estimate a row height for the grouped-overlap warning. Converted
# by hand rather than with `convertHeight()`, which would need a graphics device
# and would leave viewport records on the display list of whatever device the
# caller happened to have open. Anything that is not an absolute unit cannot be
# resolved without a device, so it falls back to the 3mm default.
core_padding_bigpts <- function(theme) {
  default <- 3 * 72 / 25.4
  pad <- theme$tab_theme$core$padding

  if(!is.unit(pad) || length(pad) < 2)
    return(default)

  out <- tryCatch({
    scale <- switch(unitType(pad[2]),
                    "mm" = 72 / 25.4,
                    "cm" = 72 / 2.54,
                    "in" = ,
                    "inches" = 72,
                    "pt" = ,
                    "points" = 72 / 72.27,
                    "bigpts" = 1,
                    NA_real_)
    as.numeric(pad[2]) * scale
  }, error = function(e) default)

  if(length(out) != 1 || !is.finite(out)) default else out
}


# Scale meta-analytic weights to point sizes.
#
# Both scalings take the square root of the weights first, so it is the *area*
# of the point that is proportional to the weight, and then map onto `range`.
# `"range"` follows the `plim` scaling of `metafor::forest.rma()`: the smallest
# weight always lands on `range[1]` and the largest on `range[2]`, which fixes
# the bounds at the cost of the areas no longer being exactly proportional.
# `"proportional"` follows `meta::forest.meta()`: areas stay proportional to the
# weights, and only the smallest points are clamped up to `floor` so that they
# do not become indistinguishable from the confidence interval line.
scale_sizes <- function(weights,
                        range = c(0.2, 0.8),
                        method = c("range", "proportional"),
                        floor = 0.35) {

  method <- match.arg(method)

  if(!is.numeric(range) || length(range) != 2 || any(is.na(range)))
    stop("`size_range` must be a numeric vector of length 2.")

  if(any(range <= 0))
    stop("`size_range` must be larger than 0.")

  if(range[1] > range[2])
    stop("`size_range` must be in increasing order.")

  w <- sqrt(weights)
  keep <- !is.na(w)

  # Nothing to scale against, leave the sizes to the caller's mid-point
  if(!any(keep))
    return(weights)

  out <- rep(NA_real_, length(w))

  if(method == "range"){
    rng <- max(w[keep]) - min(w[keep])
    if(rng <= .Machine$double.eps^0.5)
      out[keep] <- mean(range)
    else
      out[keep] <- (w[keep] - min(w[keep])) / rng * (range[2] - range[1]) + range[1]
  }else {
    out[keep] <- pmax(w[keep] / max(w[keep]), floor) * range[2]
  }

  out
}
