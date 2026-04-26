#' Pretty ticks for log-transformed axes
#'
#' Compute "log-pretty" tick values in the original (non-transformed) space
#' using decade-aware sub-multiples. Ranges spanning at least three orders of
#' magnitude collapse to one tick per decade (e.g. \code{1, 10, 100}); narrower
#' ranges include classic engineering sub-multiples (\code{1, 2, 5} or
#' \code{1, 2, 3, 5, 7}); ranges narrower than half a decade fall back to
#' \code{\link[base]{pretty}} on the original scale because dense log ticks
#' look clustered there.
#'
#' @param range_orig Numeric length-2 range in the original (non-log) scale.
#'   All values must be strictly positive; otherwise the function falls back
#'   to \code{\link[base]{pretty}}.
#' @param base Logarithm base. Use \code{exp(1)} for natural log, \code{2} for
#'   \code{log2}, or \code{10} for \code{log10}.
#'
#' @return A numeric vector of tick values in the original scale.
#'
#' @keywords internal
log_pretty <- function(range_orig, base = 10){

  if(any(!is.finite(range_orig)) || any(range_orig <= 0))
    return(pretty(range_orig))

  log_min <- log(range_orig[1], base = base)
  log_max <- log(range_orig[2], base = base)
  span    <- log_max - log_min

  sub <- if(span >= 3)        1
         else if(span >= 1.5) c(1, 2, 5)
         else if(span >= 0.5) c(1, 2, 3, 5, 7)
         else                 return(pretty(range_orig))

  k    <- seq(floor(log_min), ceiling(log_max))
  cand <- as.vector(outer(sub, base ^ k))
  cand <- sort(unique(cand))
  cand <- cand[cand >= range_orig[1] & cand <= range_orig[2]]

  if(length(cand) == 0L) pretty(range_orig) else cand
}


#' Set x-axis ticks
#'
#' Pick tick positions in the (already-transformed) \code{xlim} space.
#'
#' For \code{x_trans} in \code{"none"} / \code{"scientific"} this delegates to
#' \code{\link[base]{pretty}}. For \code{"log"} / \code{"log2"} / \code{"log10"}
#' it converts \code{xlim} back to the original scale, runs
#' \code{\link{log_pretty}} to get base-aware ticks (e.g. \code{0.1, 1, 10}
#' rather than \code{0.22, 0.61, 2.72}), and re-applies the transform. The
#' reference line value is included as a tick when it falls inside the range,
#' so forest plots always label their visual anchor.
#'
#' @param at Numerical vector, create ticks at given values.
#' @inheritParams forest
#'
#' @return A vector of tick coordinates in the transformed space.
#'
#' @keywords internal
make_ticks <- function(at = NULL,
                       xlim,
                       refline = 1,
                       x_trans = "none"){

  if(is.null(at)){

    if(x_trans %in% c("none", "scientific")){
      ticks_at <- pretty(xlim)
    }else {
      base <- switch(x_trans, log = exp(1), log2 = 2, log10 = 10)
      range_orig <- xscale(xlim, scale = x_trans, type = "inv")
      ticks_orig <- log_pretty(range_orig, base = base)

      # Anchor the reference line as a tick when it sits inside the range —
      # forest readers rely on the value at refline being explicitly labeled.
      if(is.finite(refline) && refline > 0 &&
         refline >= range_orig[1] && refline <= range_orig[2])
        ticks_orig <- sort(unique(c(ticks_orig, refline)))

      ticks_at <- xscale(ticks_orig, scale = x_trans, type = "scale")
    }

  }else {
    ticks_at <- xscale(at, scale = x_trans)
  }

  ticks_at <- ticks_at[is.finite(ticks_at)]
  ticks_at[ticks_at >= min(xlim) & ticks_at <= max(xlim)]

}


#' Create xlim
#'
#' Create xlim based on value ranges.
#'
#' @inheritParams forest
#'
#' @return A list
#'
#' @keywords internal
#'
make_xlim <- function(xlim = NULL,
                      lower,
                      upper,
                      ref_line = ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0),
                      ticks_at = NULL,
                      x_trans = "none"){

  # Use range if missing
  if(is.null(xlim)){
    xlim <- range(c(min(unlist(lower), na.rm = TRUE),
                    ref_line,
                    max(unlist(upper), na.rm = TRUE)),
                  na.rm = TRUE)
  }

  if(x_trans %in% c("log", "log2", "log10")){
    if(min(xlim) == 0)
      xlim[which.min(xlim)] <- min(c(unlist(lower),
                                     ref_line,
                                     ticks_at),
                                   na.rm = TRUE)
    xlim <- xscale(xlim, scale = x_trans)

  }

  return(xlim)

}

# Stop if not a gpar() object
# internal
stop_ifnot_gpar <- function(x) {
  nm <- deparse(substitute(x))
  if(!inherits(x, "gpar"))
    stop(sprintf("%s must be a gpar() object", nm))
}

