#' @title Identify Maximal Facets of the Convex Frontier
#'
#' @description
#' Identifies the maximal subsets of efficient Decision-Making Units (DMUs)
#' that lie on the same face (facet) of the efficient frontier. It relies on
#' the concept of "maximal friends" to determine which efficient units can
#' be grouped together to form convex combinations under the chosen returns-to-scale assumption.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables used in the model.
#' @param x Integer vector indicating the column indexes of the input variables in \code{data}.
#' @param y Integer vector indicating the column indexes of the output variables in \code{data}.
#' @param RTS Text string or number defining the underlying DEA technology /
#'   returns-to-scale assumption (default: \code{"vrs"}). Accepted values:
#'   \describe{
#'     \item{\code{0} / \code{"fdh"}}{Free disposability hull, no convexity assumption.}
#'     \item{\code{1} / \code{"vrs"}}{Variable returns to scale, convexity and free disposability.}
#'     \item{\code{2} / \code{"drs"}}{Decreasing returns to scale, convexity, down-scaling and free disposability.}
#'     \item{\code{3} / \code{"crs"}}{Constant returns to scale, convexity and free disposability.}
#'     \item{\code{4} / \code{"irs"}}{Increasing returns to scale (up-scaling, not down-scaling), convexity and free disposability.}
#'     \item{\code{5} / \code{"add"}}{Additivity (scaling up and down, but only with integers), and free disposability.}
#'   }
#'
#' @importFrom deaR make_deadata maximal_friends
#'
#' @return A \code{matrix} where each row represents a maximal facet, and the values are the row indices of the efficient DMUs in \code{data} that form that facet. Returns an empty \code{matrix} if there are no efficient units.

convex_facets <- function (
    data, x, y, RTS = "vrs"
) {

  # ----------------------------------------------------------------------------
  # Determine the efficient combinations by Additive DEA -----------------------
  # ----------------------------------------------------------------------------

  if (length(which(data$class_efficiency == "efficient")) == 0) {

    results_convx <- matrix(nrow = 0, ncol = 0)

  } else {

    # Obtain all possible combinations of maximal friends (facets)
    datadea <- make_deadata(
      data,
      inputs = x,
      outputs = y
    )

    eff_convex_list <- maximal_friends(
      datadea = datadea,
      rts = RTS,
      dmu_ref = which(data$class_efficiency == "efficient"))

    # Methodological choice: We only retain the facets with the maximum
    # number of vertices (i.e. full-dimensional facets in the dataset).
    long <- lengths(eff_convex_list)
    max_long <- max(long)

    list_max_facets <- eff_convex_list[long == max_long]

    # Combine into a matrix where each row represents a facet and
    # columns represent the indices of the DMUs forming it
    results_convx <- do.call(rbind, list_max_facets)

  }

  return(results_convx)

}
