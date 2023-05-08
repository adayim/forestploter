

#' Checking error for forest plot
#'
#' @inheritParams forest
#'
#' @keywords internal
#'
check_errors <- function(data,
                         est,
                         lower,
                         upper,
                         sizes,
                         ref_line,
                         vert_line,
                         ci_column,
                         is_summary,
                         xlim,
                         x_trans,
                         ticks_at,
                         ticks_digits,
                         title,
                         arrow_lab,
                         xlab){

  if(!is.numeric(ci_column))
    stop("ci_column must be numeric atomic vector.")

  if(!is.null(title) && length(title) != 1)
      stop("title must be of length 1.")

  # Check length
  if(length(unique(c(length(est), length(lower), length(upper)))) != 1)
    stop("Estimate, lower and upper should have the same length.")
  
  if(inherits(sizes, "list") & length(est) != length(sizes))
    stop("sizes should have the same length as est.")
  
  if(!is.numeric(unlist(sizes)))
    stop("Sizes must be numeric.")

  # Check size value
  if(any(unlist(sizes) <= 0, na.rm = TRUE))
    stop("Sizes must be larger than 0.")

  # Check type
  if(typeof(est) != typeof(lower) | typeof(est) != typeof(upper))
    stop("Estimate, lower and upper should have the same type.")
  
  if(!is.numeric(unlist(est)) | !is.numeric(unlist(lower)) | !is.numeric(unlist(upper)))
    stop("Estimate, lower and upper must be numeric.")

  if(inherits(est, "list") | inherits(lower, "list") | inherits(upper, "list")){
    est_len <- vapply(est, length, FUN.VALUE = 1L)
    lower_len <- vapply(lower, length, FUN.VALUE = 1L)
    upper_len <- vapply(upper, length, FUN.VALUE = 1L)

    if(length(unique(c(est_len, lower_len, upper_len))) != 1)
      stop("All the elements in estimate, lower and upper should have the same length")
    
    if(inherits(sizes, "list") & length(unique(c(est_len, vapply(sizes, length, FUN.VALUE = 1L)))) != 1)
      stop("All the elements in sizes should have the same length as estimate")
  }

  # Check length for the summary
  if(!is.null(is_summary) && length(is_summary) != nrow(data))
    stop("is_summary should have same legnth as data rownumber.")
  
  if(!is.null(is_summary) && ! is.logical(is_summary))
    stop("is_summary must be logical vector.")

  # Check ref_line
  if(!is.numeric(ref_line) || !length(ref_line) %in% c(1, length(ci_column)))
    stop("ref_line should be of length 1 or the same length as ci_column.")
  
  # Check the x_trans
  if(!all(x_trans %in% c("none", "log", "log2", "log10")) || !length(x_trans) %in% c(1, length(ci_column)))
    stop("x_trans must be in \"none\", \"log\", \"log2\", \"log10\" and of length 1 or the same length as ci_column.")

  # Check the xlab
  if(!is.null(xlab) && !length(xlab) %in% c(1, length(ci_column)))
    stop("xlab must be of length 1 or the same length as ci_column.")

  # Check tick_digits
  if(!is.null(ticks_digits)){
    if(!length(ticks_digits) %in% c(1, length(ci_column)))
      stop("ticks_digits must be length of 1 or same length as ci_column.")
  
    if(!is.numeric(unlist(ticks_digits)))
        stop("ticks_digits must be numeric.")
  }


  # If only one CI column
  if(length(ci_column) == 1){

    # Check vertical line
    if(!is.null(vert_line) && !is.numeric(vert_line))
      stop("vert_line must be a numeric vector.")

    # Check arrow
    if(!is.null(arrow_lab) & length(arrow_lab) != 2)
      stop("Arrow label must of length 2.")

    # Check xlim
    if(!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]))
      stop("xlim must be numeric and of length 2, with first element less than the second.")

    # Check the break
    if(!is.null(ticks_at) && !is.numeric(ticks_at))
      stop("ticks_at must be numeric.")

    if(!is.null(ticks_at) && !is.null(xlim)){
      if(max(ticks_at) > max(xlim) || min(ticks_at) < min(xlim))
        warning("ticks_at is outside the xlim.")
    }

  }else{

    # Check vertical line
    if(!is.null(vert_line)){
      if(inherits(vert_line, "list")){
        if(length(vert_line) != length(ci_column))
          stop("vert_line must have the same length as ci_column.")
        cl <- sapply(vert_line, is.numeric)
        if(any(!cl))
          stop("vert_line must be all numeric.")
      }else {
        if(!is.numeric(vert_line))
          stop("vert_line must be a numeric vector.")
      }
    }

    # Check arrow
    if(!is.null(arrow_lab)){
      if(inherits(arrow_lab, "list")){
        if(length(arrow_lab) != length(ci_column))
          stop("arrow_lab must have the same length as ci_column.")
        cl <- sapply(arrow_lab, length) == 2
        if(any(!cl))
          stop("Elements in the arrow_lab must of length 2.")
      }else {
        if(!is.null(arrow_lab) & length(arrow_lab) != 2)
          stop("Arrow label must of length 2.")
      }
    }

    # Check xlim
    if(!is.null(xlim)){
      if(inherits(xlim, "list")){
        if(length(xlim) != length(ci_column))
          stop("xlim must have the same length as ci_column.")
        tst <- sapply(xlim, function(x){
          !is.numeric(x) || length(x) != 2 || x[1] >= x[2]
        })
        if(any(tst))
          stop("Elements in the xlim must be numeric and of length 2, with first element less than the second.")

      }else {
        if(!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2])
          stop("xlim must be numeric and of length 2, with first element less than the second.")
      }
    }

    # Check the break
    if(!is.null(ticks_at)){
      if(inherits(ticks_at, "list")){
        if(length(ticks_at) != length(ci_column))
          stop("ticks_at must have the same length as ci_column.")

        cl <- sapply(ticks_at, is.numeric)
        if(any(!cl))
          stop("Elements in the ticks_at must be numeric.")

      }else {
        if(!is.numeric(ticks_at))
          stop("ticks_at must be numeric.")
      }
    }

  }

}
