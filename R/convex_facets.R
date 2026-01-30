#' @title Create New SMOTE Units to Balance Data combinations of m + s
#'
#' @description This function creates new DMUs to address data imbalances. If the majority class is efficient, it generates new inefficient DMUs by worsering the observed units. Conversely, if the majority class is inefficient, it projects inefficient DMUs to the frontier. Finally, a random selection if performed to keep a proportion of 0.65 for the majority class and 0.35 for the minority class.
#'
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param x Column indexes of the input variables in the \code{data}.
#' @param y Column indexes of the output variables in the \code{data}.
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
#'
#' @return It returns a \code{data.frame} with the newly created set of DMUs incorporated.

convex_facets <- function (
    data, x, y, RTS = "vrs"
) {

  # save a copy
  copy_data <- data

  # determine samples in a batch
  units_batch <- 10000

  # first, create set combinations
  # determinate efficient untis
  data_eff <- data[data$class_efficiency == "efficient", ]

  # number of efficient units
  n_eff <- 1:nrow(data_eff)

  # proportion importance
  len <- length(c(x,y))

  # weight lambda
  prop_imp <- 1/len

  # create lambda
  lambda <- rep(prop_imp, ncol(data_eff[, c(x,y)]))

  save_idx_eff <- NULL
  save_idx_ineff <- NULL

  # --------------------------------------------------------------------------
  # Determine the efficient combinations by Additive DEA ---------------------
  # --------------------------------------------------------------------------

  if (length(which(data$class_efficiency == "efficient")) == 0) {

    results_convx <- NA

  } else {

    datadea <- make_deadata(
      data,
      inputs = x,
      outputs = y
    )

    eff_convex_list <- maximal_friends(
      datadea = datadea,
      rts = RTS,
      dmu_ref = which(data$class_efficiency == "efficient"))

    long <- lengths(eff_convex_list)
    max_long <- max(long)

    list_max_facets <- eff_convex_list[long == max_long]

    results_convx <- as.data.frame(do.call(rbind, list_max_facets))
    results_convx <- as.matrix(results_convx)

  }

  return(results_convx)

}
