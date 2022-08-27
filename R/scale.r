#' Change Axis Scale: log2, log10 and more
#'
#' Change axis scale.
#' 
#' @param scale axis scale. Allowed values are one of c("none", "log2", "log10",
#'  "sqrt", "percent", "dollar", "scientific"); e.g.: .scale="log2".
#' @param inv Inverse value back to its orignal.
#' @param format ogical value. If TRUE, axis tick mark labels will be formatted
#'  when scale  = "log2" or "log10".
#' @param format_digits Digits to keep while formating
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
                log2 = math_exp(x, expr = 2^x),
                log10 = math_exp(x, expr = 10^x),
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

# Math expression
math_exp <- function(x, expr = 10^x){

  expr_qt <- substitute(expr)
  subs <- function(x) {
    do.call("substitute", list(expr_qt, list(x = x)))
  }

  f <- function(x) {
    ret <- lapply(x, subs)
    ret <- as.expression(ret)
    # restore NAs from input vector
    ret[is.na(x)] <- NA
    names(ret) <- names(x)
    ret
  }

  r <- f(x)
  r[x == 0] <- 1
  
  return(r)

}


