#' @title Friends check.
#'
#' @description Checks whether a subset of DMUs is friends or not, according to Tone (2010).
#'
#' @usage is.friends(datadea,
#'              dmu_eval = NULL,
#'              dmu_ref = NULL,
#'              rts = c("crs", "vrs", "nirs", "ndrs"),
#'              tol = 1e-6)
#'
#' @param datadea The data, including \code{n} DMUs, \code{m} inputs and \code{s} outputs.
#' @param dmu_eval A numeric vector containing the subset of DMUs to be checked.
#'                 If \code{NULL} (default), all DMUs are considered.
#' @param dmu_ref A numeric vector containing which DMUs are the evaluation reference set.
#'                If \code{NULL} (default), all DMUs are considered.
#' @param rts A string, determining the type of returns to scale, equal to "crs" (constant),
#'            "vrs" (variable), "nirs" (non-increasing) or "ndrs" (non-decreasing).
#' @param tol Numeric, a tolerance margin for checking efficiency. It is 1e-6 by default.
#'
#' @returns Returns \code{TRUE} if \code{dmu_eval} is friends of \code{dmu_ref},
#'         and \code{FALSE} otherwise.
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
#' @references
#' Tone, K. (2010). "Variations on the theme of slacks-based measure of efficiency
#' in DEA", European Journal of Operational Research, 200, 901-907.
#' \doi{10.1016/j.ejor.2009.01.027}
#'
#' @examples
#' data("PFT1981")
#' datadea <- make_deadata(PFT1981,
#'                         ni = 5,
#'                         no = 3)
#' subset1 <- c(15, 16, 17, 19) # Subset of DMUs to be checked
#' result1 <- is.friends(datadea = datadea,
#'                       dmu_eval = subset1,
#'                       dmu_ref = 1:20) # We only consider a cluster formed by the first 20 DMUs
#' subset2 <- c(15, 16, 17, 20) # Another subset of DMUs to be checked
#' result2 <- is.friends(datadea = datadea,
#'                       dmu_eval = subset2,
#'                       dmu_ref = 1:20) # We only consider a cluster formed by the first 20 DMUs
#'
#' @seealso \code{\link{maximal_friends}}, \code{\link{model_sbmeff}}
#'
#' @import lpSolve
#'
#' @export

is.friends <- function(datadea,
                       dmu_eval = NULL,
                       dmu_ref = NULL,
                       rts = c("crs", "vrs", "nirs", "ndrs"),
                       tol = 1e-6) {

  # Cheking whether datadea is of class "deadata" or not...
  if (!is.deadata(datadea)) {
    stop("Data should be of class deadata. Run make_deadata function first!")
  }

  # Checking rts
  rts <- tolower(rts)
  rts <- match.arg(rts)

  nd <- length(datadea$dmunames) # number of dmus

  if (is.null(dmu_eval)) {
    dmu_eval <- 1:nd
  } else if (!all(dmu_eval %in% (1:nd))) {
    stop("Invalid set of DMUs to be evaluated (dmu_eval).")
  }
  nde <- length(dmu_eval)

  if (is.null(dmu_ref)) {
    dmu_ref <- 1:nd
  } else if (!all(dmu_ref %in% (1:nd))) {
    stop("Invalid set of reference DMUs (dmu_ref).")
  }
  ndr <- length(dmu_ref)

  input <- datadea$input
  output <- datadea$output
  ni <- nrow(input) # number of  inputs
  no <- nrow(output) # number of outputs

  inputref <- matrix(input[, dmu_ref], nrow = ni)
  outputref <- matrix(output[, dmu_ref], nrow = no)

  inputeval <- matrix(input[, dmu_eval], nrow = ni)
  outputeval <- matrix(output[, dmu_eval], nrow = no)
  inputtest = apply(inputeval, MARGIN = 1, FUN = sum) / nde
  outputtest = apply(outputeval, MARGIN = 1, FUN = sum) / nde

  datadeatest <- structure(list(
    input = cbind(input, matrix(inputtest, nrow = ni)),
    output = cbind(output, matrix(outputtest, nrow = no)),
    dmunames = c(datadea$dmunames, "DMU_Test"),
    nc_inputs = datadea$nc_inputs,
    nc_outputs = datadea$nc_outputs,
    nd_inputs = datadea$nd_inputs,
    nd_outputs = datadea$nd_outputs,
    ud_inputs = datadea$ud_inputs,
    ud_outputs = datadea$ud_outputs
  ), class = "deadata")

  # result_sbm <- model_sbmeff(datadea = datadeatest,
  #                            dmu_ref = c(dmu_ref, nd + 1),
  #                            dmu_eval = nd + 1,
  #                            rts = rts)
  # eff <- result_sbm$DMU[[1]]$efficiency

  result_add <- model_additive(datadea = datadeatest,
                               dmu_ref = c(dmu_ref, nd + 1),
                               dmu_eval = nd + 1,
                               rts = rts)
  objval <- result_add$DMU[[1]]$objval

  if (is.numeric(objval)) {
    slacks_input <- result_add$DMU[[1]]$slack_input / datadeatest$input[, nd + 1]
    slacks_output <- result_add$DMU[[1]]$slack_output / datadeatest$output[, nd + 1]
    if (any(c(slacks_input, slacks_output) >= tol)) {
      #if (objval >= tol) {
      eff <- 0
    } else {
      eff <- 1
    }
  } else {
    result_radial <- model_basic(datadea = datadeatest,
                                 dmu_ref = c(dmu_ref, nd + 1),
                                 dmu_eval = nd + 1,
                                 rts = rts)
    eff <- result_radial$DMU[[1]]$efficiency
    slacks_input <- result_radial$DMU[[1]]$slack_input / datadeatest$input[, nd + 1]
    slacks_output <- result_radial$DMU[[1]]$slack_output / datadeatest$output[, nd + 1]
    if ((is.numeric(eff)) && (eff >= 1 - tol) && any(c(slacks_input, slacks_output) >= tol)) eff <- 0
  }

  if (!is.numeric(eff)) {
    eff <- 0
    warning("Error in the computation of SBM efficiency inside is.friends with DMUs ", toString(dmu_eval))
  }

  return(eff >= (1 - tol))

}
