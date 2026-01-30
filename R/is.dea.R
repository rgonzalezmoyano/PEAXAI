#' @title dea class check.
#'
#' @description Checks whether an R object is of dea class or not.
#' @usage is.dea(x)
#'
#' @param x Any \bold{R} object.
#'
#' @returns Returns \code{TRUE} if its argument is a \code{dea} object (that is, has "dea"
#'   amongst its classes) and \code{FALSE} otherwise.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Vicente Bolós} (\email{vicente.bolos@@uv.es}).
#' \emph{Department of Business Mathematics}
#'
#' \strong{Rafael Benítez} (\email{rafael.suarez@@uv.es}).
#' \emph{Department of Business Mathematics}
#'
#' University of Valencia (Spain)
#'
#' @export


is.dea <- function(x) {

  inherits(x, "dea")

}
