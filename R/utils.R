
#' Set x-axis
#'
#' @param break_at Numerical vector, create ticks at given values.
#' @param label_at Optional, numerical vector. Put labels at given values. The
#' value of \code{break_at} will be used if not provided.
#' @param label_value Optional, character or numeric vector to put labels for the
#' the x-axis. Should have same length as the \code{label_at}. The value of the
#' value of \code{label_at} will be used if not provided.
#'
#' @return A list.
#'
#' @keywords deprecated
#'
set_xaxis <- function(break_at, label_at = NULL, label_value = NULL){

  if(!is.numeric(break_at) || (!missing(label_at) && !is.numeric(label_at)))
    stop("break_at and label_at must be a numeric vector.")

  if(is.null(label_at))
    label_at <- break_at

  if(!is.null(label_at) && is.null(label_value))
    label_value <- label_at

  if(!is.null(label_value) && is.null(label_at))
    stop("label_at not provided.")

  if(!is.null(label_at) && !is.null(label_value)){
    if(length(label_at) != length(label_value))
      stop("label_at and label_value should have same length.")

    if(max(label_at) > max(break_at) || min(label_at) < min(break_at))
      stop("label_at should be within break_at")
  }

  r <- list(break_at = break_at, label_at = label_at, label_value = label_value)

  class(r) <- union("xaxis", r)

  return(r)

}


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
                       refline = ifelse(is_exp, 1, 0),
                       is_exp = FALSE){

  if(is.null(at)){

    if(!is_exp){
      ticks_at <- pretty(xlim)
    }else {
      pt_cut <- pretty(range(c(xlim, refline)))
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
#' @param gp_num Group number
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
                      gp_num = 1,
                      ref_line = ifelse(is_exp, 1, 0),
                      ticks_at = NULL,
                      is_exp = FALSE){

  if(gp_num == 1 || !is.null(xlim)){

    # Use range if missing
    if(is.null(xlim)){
      xlim <- range(c(unlist(lower),
                      ifelse(is_exp, exp(ref_line), ref_line),
                      unlist(upper)),
                    na.rm = TRUE)
    }
      
    if(is_exp){
      if(min(xlim) == 0)
        xlim[which.min(xlim)] <- min(c(unlist(lower),
                                       exp(ref_line),
                                       ticks_at),
                                     na.rm = TRUE)
      xlim <- log(xlim)

    }

    xlim <- list(xlim)

  }else{

    gp_list <- rep_len(1:gp_num, length(lower))

    xlim <- lapply(1:gp_num, function(x){
      sel_num <- gp_list == x

      xlim <- range(c(unlist(lower[sel_num]),
                      ifelse(is_exp, exp(ref_line), ref_line),
                      unlist(upper[sel_num])),
                    na.rm = TRUE)

      if(is_exp){
        if(min(xlim) == 0)
          xlim[which.min(xlim)] <- min(c(unlist(lower[sel_num]),
                                         exp(ref_line),
                                         ticks_at),
                                       na.rm = TRUE)
        xlim <- log(xlim)

      }
      return(xlim)

    })

  }

  return(xlim)

}

