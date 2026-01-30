#' @title Maximal friends of a set of DMUs.
#'
#' @description Finds the maximal friends subsets of a given set of DMUs, according to Tone (2010).
#'              It uses an ascending algorithm in order to find directly maximal subsets.
#'
#' @usage maximal_friends(datadea,
#'              dmu_ref = NULL,
#'              rts = c("crs", "vrs", "nirs", "ndrs"),
#'              tol = 1e-6,
#'              silent = FALSE)
#'
#' @param datadea A \code{deadata} object with \code{n} DMUs, \code{m} inputs and \code{s} outputs.
#' @param dmu_ref A numeric vector containing which DMUs are the evaluation reference set,
#'                i.e. the cluster of DMUs from which we want to find maximal friends.
#'                If \code{NULL} (default), all DMUs are considered.
#' @param rts A string, determining the type of returns to scale, equal to "crs" (constant),
#'            "vrs" (variable), "nirs" (non-increasing) or "ndrs" (non-decreasing).
#' @param tol Numeric, a tolerance margin for checking efficiency. It is 1e-6 by default.
#' @param silent Logical, if \code{FALSE} (default) steps are printed.
#'
#' @returns A list with numeric vectors representing maximal friends subsets of DMUs.
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
#' Tone, K. (2010). "Variations on the theme of slacks-based measure of efficiency in DEA",
#' European Journal of Operational Research, 200, 901-907. \doi{10.1016/j.ejor.2009.01.027}
#'
#' @examples
#' \dontrun{
#' data("PFT1981")
#' datadea <- make_deadata(PFT1981,
#'                         ni = 5,
#'                         no = 3)
#' # We find maximal friends of a cluster formed by the first 20 DMUs
#' result <- maximal_friends(datadea = datadea,
#'                           dmu_ref = 1:20)
#' }
#'
#' @seealso \code{\link{is.friends}}, \code{\link{model_sbmeff}}
#'
#' @import lpSolve
#'
#' @export

maximal_friends <- function(datadea,
                            dmu_ref = NULL,
                            rts = c("crs", "vrs", "nirs", "ndrs"),
                            tol = 1e-6,
                            silent = FALSE) {

  # Cheking whether datadea is of class "deadata" or not...
  if (!is.deadata(datadea)) {
    stop("Data should be of class deadata. Run make_deadata function first!")
  }

  # Checking rts
  rts <- tolower(rts)
  rts <- match.arg(rts)

  dmunames <- datadea$dmunames
  nd <- length(datadea$dmunames) # number of dmus

  input <- datadea$input
  output <- datadea$output
  ni <- nrow(input) # number of  inputs
  no <- nrow(output) # number of outputs

  if (is.null(dmu_ref)) {
    dmu_ref <- 1:nd
  } else if (!all(dmu_ref %in% (1:nd))) {
    stop("Invalid set of reference DMUs (dmu_ref).")
  }
  names(dmu_ref) <- dmunames[dmu_ref]

  # Find efficient DMUs in dmu_ref
  result_add <- model_additive(datadea = datadea,
                               dmu_eval = dmu_ref,
                               dmu_ref = dmu_ref,
                               rts = rts)
  objval <- unlist(lapply(result_add$DMU, function(x) x$objval))
  slacksio <- slacks(result_add)
  slacks_input <- t(slacksio$slack_input) / datadea$input[, dmu_ref]
  slacks_output <- t(slacksio$slack_output) / datadea$output[, dmu_ref]
  slacks_matrix <- rbind(slacks_input, slacks_output)
  objval <- colSums(slacks_matrix)
  effDMUs <- dmu_ref[which(objval <= tol)]
  ne <- length(effDMUs)

  input_eff <- matrix(input[, effDMUs], nrow = ni)
  output_eff <- matrix(output[, effDMUs], nrow = no)
  datadeaeff <- structure(list(
    input = input_eff,
    output = output_eff,
    dmunames = dmunames[effDMUs],
    nc_inputs = datadea$nc_inputs,
    nc_outputs = datadea$nc_outputs,
    nd_inputs = datadea$nd_inputs,
    nd_outputs = datadea$nd_outputs,
    ud_inputs = datadea$ud_inputs,
    ud_outputs = datadea$ud_outputs
  ), class = "deadata")

  maxfr <- list()
  nomaxfr <- list()
  cand <- list() # candidates

  cand <- append(cand, 1:ne)
  #cand <- lapply(1:ne, FUN = function(x){x}) # This is equivalent

  for (k in seq_len(ne - 1)) {

    if (!silent) {
      print(paste("Computing maximal friends with", toString(k), "DMUs (step",
                  toString(k), "of", toString(ne - 1), ")"))
    }

    for (kk in seq_along(cand)) {

      aux <- TRUE
      jk <- cand[[kk]][k] # last element of cand[[kk]]
      if (jk < ne) {
        for (j in seq(from = jk + 1, to = ne)) {
          dmu_eval <- c(cand[[kk]], j)
          if (is.friends(datadea = datadea, dmu_eval = effDMUs[dmu_eval], dmu_ref = effDMUs, rts = rts)) {
            nomaxfr <- c(nomaxfr, list(dmu_eval))
            aux <- FALSE
          }
        }
      }
      if (aux) {
        maxfr <- c(maxfr, list(effDMUs[cand[[kk]]]))
      }

    }

    cand <- nomaxfr
    nomaxfr <- list()

  }

  # If in the last step cand is not empty, there is only one facet
  if (length(cand) > 0) {
    maxfr <- list(effDMUs)
  }

  # Remove dominated friends from maxfr
  nmf <- 1:length(maxfr)
  auxmaxfr <- maxfr
  for (i in nmf) {
    for (j in nmf[-i]) {
      if (all(auxmaxfr[[i]] %in% auxmaxfr[[j]])) {
        auxmaxfr[[i]] <- 0
      }
    }
  }
  maxfr <- list()
  k <- 1
  for (i in nmf) {
    if (auxmaxfr[[i]][1] != 0) {
      maxfr[[k]] <- auxmaxfr[[i]]
      k <- k + 1
    }
  }

  names(maxfr) <- paste("Facet", seq_along(maxfr))

  return(maxfr)

}

# Descending search algorithm (slower)
#
#if (is.friends(datadea = datadeaeff, rts = rts)) {
#  maxfr[[1]] <- effDMUs
#} else {
#  nomaxfr[[1]] <- 1:ne
#
#  for (k in seq_len(ne - 2)) {
#    if (!silent) {
#      print(paste("Step", toString(k), "of", toString(ne - 2),
#                  "(computing", toString((ne - k + 1) * (length(nomaxfr))), "subsets)"))
#    }
#    cand <- nomaxfr
#    nomaxfr <- list()
#
#    for (kk in seq_along(cand)) {
#
#      for (kkk in seq_along(cand[[kk]])) {
#        dmu_eval <- cand[[kk]][-kkk]
#
#        aux <- !any(unlist(lapply(maxfr, function(x) all(effDMUs[dmu_eval] %in% x))))
#        aux <- TRUE
#        for (ii in seq_along(maxfr)) {
#          if (all(effDMUs[dmu_eval] %in% maxfr[[ii]]))
#          {
#            aux <- FALSE
#            break
#          }
#        }
#
#        if ((aux) && (!list(dmu_eval) %in% nomaxfr)) {
#          if (is.friends(datadea = datadeaeff, dmu_eval = dmu_eval, rts = rts)) {
#            maxfr <- c(maxfr, list(effDMUs[dmu_eval]))
#          } else {
#            nomaxfr <- c(nomaxfr, list(dmu_eval))
#          }
#        }
#      }
#
#    }
#
#  }
#  # Add sets of just 1 DMU that are not in maxfr
#  maxfr <- append(maxfr, effDMUs[!effDMUs %in% unique(unlist(maxfr))])
#
#}
