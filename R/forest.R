
#' Forest plot
#'
#' @param data Data to be displayed in the forest plot
#' @param est Point estimation
#' @param lower Lower bound of the confidence interval
#' @param upper Upper bound of the confidence interval
#' @param sizes Size of the point estimation box.
#' @param null.at X-axis coordinates of zero line, default is 1.
#' @param ci.col Column number of the data the CI will be displayed.
#' @param ci.width Width of the CI column in the forest plot, default is 1. This
#' is the ratio of maximum width of the columns in the data.
#' @param xlim Limits for the x axis as a vector length 2, i.e. c(low, high). It
#' will take the maximum and minimum of the \code{tick.breaks} or CI.
#' @param tick.breaks X-axis breaks points, a vector.
#' @param arrow.lab Labels for the arrows, string vector of length two (left and
#' right).
#' @param theme Theme of the forest plot, see \link[gridExtra::tableGrob] for
#' details.
#'
#' @return A gtable object.
#' @export
#'
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_padding
#' @importFrom gridExtra tableGrob
#' @importFrom grid grobHeight
#'

forest <- function(data,
                   est,
                   lower,
                   upper,
                   sizes = 0.4,
                   null.at = 1,
                   ci.col,
                   ci.width = 1,
                   xlim = NULL,
                   tick.breaks = NULL,
                   arrow.lab = NULL,
                   theme = NULL){
  # Check length
  if(length(unique(c(length(est), length(lower), length(upper), nrow(data)))) != 1)
    stop("Estimate, lower and upper should be same length as data row number.")

  # Check xlim
  if(is.null(xlim) & is.null(tick.breaks))
    xlim <- range(c(lower, upper), na.rm = TRUE)

  if(is.null(xlim) & !is.null(tick.breaks))
    xlim <- range(tick.breaks)

  # Point sizes
  if(!is.null(sizes) & length(sizes) != 1){
    sizes <- sqrt(1/sizes)
    sizes <- sizes/max(sizes, na.rm = TRUE)
  }

  if(length(sizes) == 1)
    sizes <- rep(sizes, nrow(data))


  # X-axis breaks
  if(is.null(tick.breaks))
    tick.breaks <- c(xlim[1], null.at, xlim[2])

  # Set theme
  if(is.null(theme)){
    theme <- gridExtra:::ttheme_minimal(
      core=list(
        fg_params = list(hjust = 0, x = 0.05),   # font
        bg_params = list(fill=c(rep(c("#eff3f2", "white"),
                                    length.out=4))), # bands
        padding = unit(c(4, 2.5), "mm")
      ),
      colhead = list(
        fg_params = list(hjust = 0, x = 0.05, #parse=TRUE,
                         fontface=2L),
        bg_params = list(fill = "white"),
        padding = unit(c(4, 4), "mm")
      )
    )
  }

  # Calculate width of the table and multiple ratio of the CI column
  tmp_tab <- tableGrob(data, theme = theme, rows = NULL)
  col_width <- tmp_tab$widths
  col_width[ci.col] <- ci.width*col_width[ci.col]

  # Convert data to plot
  gt <- tableGrob(data,
                  theme = theme,
                  rows = NULL,
                  width = col_width)

  # Draw CI
  for(i in 1:nrow(data)){
    if(is.na(est[i]))
      next
    ug <- makeci(est = est[i],
                 lower = lower[i],
                 upper = upper[i],
                 size = sizes[i],
                 xlim = xlim)

    gt <- gtable_add_grob(gt, ug, t = i+1, l = ci.col,
                          b = i+1, r = ci.col,
                          clip = "off")
  }

  # Add vertical line
  gt <- gtable_add_grob(gt,
                        vert_line(x = null.at, gp = gpar(lty = "dotted"),
                                  xlim = xlim),
                        t = 2,
                        l = ci.col,
                        b = nrow(gt), r = ci.col,
                        clip = "off")
  # Add X-axis
  xais <- makexaxis(at = tick.breaks,
                    x0 = null.at,
                    arrow.lab = arrow.lab,
                    xlim = xlim)

  gt <- gtable_add_rows(gt, heights = grobHeight(xais))

  gt <- gtable_add_grob(gt, xais,
                        t = nrow(gt), l = ci.col,
                        b = nrow(gt), r = ci.col,
                        clip = "off")
  # Add padding
  gt <- gtable_add_padding(gt, unit(1, "cm"))

  return(gt)

}

