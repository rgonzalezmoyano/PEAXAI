#' @title Data preprocessing and efficiency labeling with Additive DEA
#'
#' @description
#' Labels each DMU (Decision Making Unit) as efficient or not using the
#' Additive DEA model, optionally after basic data preprocessing. The resulting
#' factor \code{class_efficiency} has levels \code{c("not_efficient","efficient")},
#' where \code{"efficient"} is the positive class for downstream modeling.
#'
#' @param data A \code{data.frame} or \code{matrix} containing all variables.
#' @param REF Optional reference set of inputs that defines the technology
#'   (defaults to the columns indicated by \code{x} in \code{data}). Must have
#'   the same number of rows as \code{data}.
#' @param x Integer vector with column indices of input variables in \code{data}.
#' @param y Integer vector with column indices of output variables in \code{data}.
#' @param RTS Character or integer specifying the DEA technology / returns-to-scale
#'   assumption (default: \code{"vrs"}). Accepted values:
#'   \describe{
#'     \item{\code{0} / \code{"fdh"}}{Free disposability hull (no convexity).}
#'     \item{\code{1} / \code{"vrs"}}{Variable returns to scale (convexity + free disposability).}
#'     \item{\code{2} / \code{"drs"}}{Decreasing returns to scale (convexity, down-scaling, free disposability).}
#'     \item{\code{3} / \code{"crs"}}{Constant returns to scale (convexity + free disposability).}
#'     \item{\code{4} / \code{"irs"}}{Increasing returns to scale (up-scaling only, convexity + free disposability).}
#'     \item{\code{5} / \code{"add"}}{Additivity (integer up/down scaling) with free disposability.}
#'   }
#'
#' @details
#' Internally relies on \code{\link[Benchmarking]{dea.add}} to compute Additive DEA
#' scores and derive the binary efficiency label.
#'
#' @importFrom Benchmarking dea.add
#'
#' @return
#' A \code{data.frame} equal to \code{data} (retaining all input \code{x} and
#' output \code{y} columns) plus a new factor column \code{class_efficiency}
#' with levels \code{c("not_efficient","efficient")}.
#'
#' @seealso \code{\link[Benchmarking]{dea.add}}
#'
#' @examples
#' # Example (assuming columns 1:2 are inputs and 3 is output):
#' # out <- my_fun(data = df, x = 1:2, y = 3, RTS = "vrs")
#' # table(out$class_efficiency)
#'
#' @export

label_efficiency <- function (
    data, REF = data, x, y, RTS = "vrs"
  ) {

  # benchmarking to calculate additive-DEA
  add_scores <- dea.add(
    X = as.matrix(data[,x]),
    Y = as.matrix(data[,y]),
    XREF = as.matrix(REF[,x]),
    YREF = as.matrix(REF[,y]),
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
