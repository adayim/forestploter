
#' Scale point sizes by weights
#'
#' Read the \code{sizes} given to \code{\link{forest}} as study weights and
#' turn them into point sizes. The square root of the weights is taken first,
#' so that the \emph{area} of each point is proportional to its weight, before
#' mapping onto \code{range}.
#'
#' Weights are scaled jointly across all groups and CI columns so that the
#' areas stay comparable between them; scale by hand if per-column control is
#' wanted. Rows flagged by \code{is_summary} are held out of the scaling and
#' drawn at \code{range[2]}, since a pooled total is not comparable with a
#' study weight. This follows \code{meta}, whose pooled rows carry no study
#' weight and end up the size of the largest study square, and \code{metafor},
#' which sizes its summary polygon from \code{efac} rather than from the
#' weights.
#'
#' Each call sets both the method and the range, and \code{method = NULL}
#' turns the scaling off, so that \code{sizes} are used as they are.
#'
#' @param plot A forest plot object, see \code{\link{forest}}.
#' @param method \code{"range"} (default) puts the smallest weight on
#' \code{range[1]} and the largest on \code{range[2]}, as
#' \code{metafor::forest.rma} does with its \code{plim}.
#' \code{"proportional"} keeps the areas proportional to the weights and only
#' clamps the smallest points up, as \code{meta::forest.meta} does.
#' @param range Numeric vector of length 2 giving the smallest and largest
#' point size, as a multiple of one line of text.
#'
#' @return A forest plot object.
#' @seealso \code{\link{forest}}
#' @example inst/examples/scale-sizes-example.R
#' @export
scale_sizes <- function(plot,
                        method = c("range", "proportional"),
                        range = c(0.2, 0.8)){

  recipe <- recipe_to_update(plot, "scale_sizes")

  if(is.null(method)){
    recipe["size_scale"] <- list(NULL)
    return(build_plot(recipe))
  }

  method <- match_choice(method, c("range", "proportional"), "method")

  if(!is.numeric(range) || length(range) != 2 || any(is.na(range)))
    stop("`range` must be a numeric vector of length 2.")

  recipe$size_scale <- list(method = method, range = range)

  build_plot(recipe)
}
