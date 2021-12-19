
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
#' @export
#'
set_xaxis <- function(break_at, label_at = NULL, label_value = NULL){

  if(!is.numeric(break_at))
    stop("break_at must be a numeric vector.")

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

