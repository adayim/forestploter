

#' Checking error for forest plot
#'
#' The settings of the x-axis and the labels are checked by
#' \code{\link{set_xaxis}} and \code{\link{set_labs}}.
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
                         ci_column,
                         is_summary){

  if(!is.numeric(ci_column))
    stop("ci_column must be numeric atomic vector.")

  if(any(ci_column < 1) || any(ci_column > ncol(data)))
    stop("ci_column must be within seq_len(ncol(data)).")

  # Check length
  if(length(unique(c(length(est), length(lower), length(upper)))) != 1)
    stop("Estimate, lower and upper should have the same length.")

  if(inherits(est, "list") && length(est) %% length(ci_column) != 0)
    stop("Length of est should be a multiple of the length of ci_column.")

  if(inherits(sizes, "list") && length(est) != length(sizes))
    stop("sizes should have the same length as est.")

  if(!is.numeric(unlist(sizes)))
    stop("Sizes must be numeric.")

  # Check size value
  if(any(unlist(sizes) <= 0, na.rm = TRUE))
    stop("Sizes must be larger than 0.")

  # Check type
  if(typeof(est) != typeof(lower) || typeof(est) != typeof(upper))
    stop("Estimate, lower and upper should have the same type.")

  if(!is.numeric(unlist(est)) || !is.numeric(unlist(lower)) || !is.numeric(unlist(upper)))
    stop("Estimate, lower and upper must be numeric.")

  if(inherits(est, "list") | inherits(lower, "list") | inherits(upper, "list")){
    est_len <- vapply(est, length, FUN.VALUE = 1L)
    lower_len <- vapply(lower, length, FUN.VALUE = 1L)
    upper_len <- vapply(upper, length, FUN.VALUE = 1L)

    if(length(unique(c(est_len, lower_len, upper_len))) != 1)
      stop("All the elements in estimate, lower and upper should have the same length.")

    if(inherits(sizes, "list") && length(unique(c(est_len, vapply(sizes, length, FUN.VALUE = 1L)))) != 1)
      stop("All the elements in sizes should have the same length as estimate.")
  }

  # Check length for the summary
  if(!is.null(is_summary) && length(is_summary) != nrow(data))
    stop("is_summary should have the same length as the number of rows in data.")

  if(!is.null(is_summary) && !is.logical(is_summary))
    stop("is_summary must be a logical vector.")

  # Check ref_line, `NULL` follows the scale of the x-axis
  if(!is.null(ref_line) && (!is.numeric(ref_line) || !length(ref_line) %in% c(1, length(ci_column))))
    stop("ref_line should be of length 1 or the same length as ci_column.")

}
