#' @title Data Preprocessing and Efficiency Labeling Using DEA
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
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
#' @importFrom Benchmarking dea.add
#'
#' @return A \code{data.frame} identical to \code{data} (keeping all input \code{x} and output \code{y} columns) plus a new factor column \code{class_efficiency} with levels \code{c("not_efficient","efficient")}. The second level (\code{"efficient"}) is the positive class used in modeling.
#'
#' @export

label_efficiency <- function (
    data, x, y, RTS = "vrs"
    ) {

  # benchmarking to calculate additive-DEA
  add_scores <- dea.add(
    X = as.matrix(data[,x]),
    Y = as.matrix(data[,y]),
    XREF = as.matrix(data[,x]),
    YREF = as.matrix(data[,y]),
    RTS = RTS
  )[["sum"]]

  # determine efficient and inefficient DMUs
  class_efficiency <- ifelse(add_scores <= 0.0001, 1, 0)

  # add new varaible
  data <- as.data.frame (
    cbind(data, class_efficiency)
  )

  # labels
  data$class_efficiency <- ifelse(
    data$class_efficiency == 1, "efficient", "not_efficient")

  # create the 'class_efficiency' in a factor
  data$class_efficiency <- factor(data$class_efficiency)

  # order of levels
  data$class_efficiency <- factor(
    data$class_efficiency,
    levels = c("efficient", "not_efficient"))

  return(data)

}
