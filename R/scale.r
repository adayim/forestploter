#' Apply, invert, or format an x-axis scale
#'
#' Helper used by the forest plot to switch between the user-facing axis
#' scale and the internal numeric scale, and to format tick labels.
#'
#' @param x Numeric vector to be transformed or formatted.
#' @param scale Axis scale. One of \code{"none"}, \code{"log"}, \code{"log2"},
#' \code{"log10"}, or \code{"scientific"}.
#' @param type What to do with \code{x}: \code{"scale"} applies the
#' transformation, \code{"inv"} inverts it back to the original space, and
#' \code{"format"} returns formatted character labels.
#' @param format_digits Number of digits to keep when \code{type = "format"}.
#' If an integer is supplied (e.g. \code{1L}) trailing zeros are dropped.
#' @keywords internal

xscale <- function(x,
                   scale = c("none", "log", "log2", "log10", "scientific"),
                   type = c("scale", "inv", "format"),
                   format_digits = 1){

  scale <- match.arg(scale)
  type <- match.arg(type)

  # Convert values
  if(type == "scale"){
    r <- switch(scale,
                log2 = log(x, 2),
                log10 = log(x, 10),
                none = x,
                log = log(x),
                scientific = x
                )
    
    return(r)
  }

  # Format value
  if(type == "format"){
    r <- switch(scale,
                log2 = trimws(formatC(2^x, format="f", digits = format_digits, drop0trailing = is.integer(format_digits))),
                log10 = trimws(formatC(10^x, format="f", digits = format_digits, drop0trailing = is.integer(format_digits))),
                none = trimws(formatC(x, format="f", digits = format_digits, drop0trailing = is.integer(format_digits))),
                log = trimws(formatC(exp(x), format="f", digits = format_digits, drop0trailing = is.integer(format_digits))),
                scientific = trimws(formatC(x, format="e", digits = format_digits, drop0trailing = is.integer(format_digits)))
                )
    
    return(r)
  }

  # Inverse
  if(type == "inv"){
    r <- switch(scale,
                log2 = 2^x,
                log10 = 10^x,
                none = x,
                log = exp(x),
                scientific = x
                )
    
    return(r)
  }
}

