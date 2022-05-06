#' Set x-axis ticks
#'
#' Create ticks points.
#'
#' @param at Numerical vector, create ticks at given values.
#' @param is_exp If values is exponential.
#' @inheritParams forest
#'
#' @return A vector
#'
#' @keywords internal

make_ticks <- function(at = NULL,
                       xlim,
                       refline = 1,
                       is_exp = FALSE){

  if(is.null(at)){

    if(!is_exp){
      ticks_at <- pretty(xlim)
    }else {
      pt_cut <- pretty(range(c(xlim, log(refline))))
      pt_cut <- round(exp(pt_cut), 1) # Keep 1 digits
      ticks_at <- log(unique(pt_cut)) # avoid any duplicate
      # Limit values inside xlim
      ticks_at <- ticks_at[exp(ticks_at) <= max(exp(xlim))]
    }

  }else {
    if(is_exp)
      ticks_at <- log(at)
    else
      ticks_at <- at
  }

  ticks_at <- ticks_at[is.finite(ticks_at)]
  ticks_at[ticks_at >= min(xlim) & ticks_at <= max(xlim)]

}


#' Create xlim
#'
#' Create xlim based on value ranges.
#'
#' @param is_exp If values is exponential.
#' @inheritParams forest
#'
#' @return A list
#'
#' @keywords internal
#'
make_xlim <- function(xlim = NULL,
                      lower,
                      upper,
                      ref_line = ifelse(is_exp, 1, 0),
                      ticks_at = NULL,
                      is_exp = FALSE){

  # Use range if missing
  if(is.null(xlim)){
    xlim <- range(c(min(unlist(lower), na.rm = TRUE),
                    ref_line,
                    max(unlist(upper), na.rm = TRUE)),
                  na.rm = TRUE)
  }

  if(is_exp){
    if(min(xlim) == 0)
      xlim[which.min(xlim)] <- min(c(unlist(lower),
                                     ref_line,
                                     ticks_at),
                                   na.rm = TRUE)
    xlim <- log(xlim)

  }

  return(xlim)

}

