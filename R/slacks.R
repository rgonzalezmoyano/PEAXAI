#' @title Slacks
#'
#' @description Extract the slacks of the DMUs from a \code{dea} or \code{dea_fuzzy} solution.
#'
#' @usage slacks(deasol)
#'
#' @param deasol Object of class \code{dea} or \code{dea_fuzzy} obtained with some
#' of the DEA model functions.
#'
#' @returns A list with the optimal slacks for each evaluated DMU.
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
#' @examples
#' data("Coll_Blasco_2006")
#' data_example <- make_deadata(Coll_Blasco_2006,
#'                              ni = 2,
#'                              no = 2)
#' result <- model_multiplier(data_example,
#'                            orientation = "io",
#'                            rts = "crs")
#' slacks(result)
#'
#' @export

slacks <- function(deasol) {

  slacklist <- NULL

  if (is.dea(deasol)) {

    slack_input <- NULL
    if ("slack_input" %in% names(deasol$DMU[[1]])) {
      slack_input <- do.call(rbind, lapply(deasol$DMU, function(x)
        x$slack_input))
    }

    slack_output <- NULL
    if ("slack_output" %in% names(deasol$DMU[[1]])) {
      slack_output <- do.call(rbind, lapply(deasol$DMU, function(x)
        x$slack_output))
    }

    t_input <- NULL
    t_output <- NULL
    if (deasol$modelname %in% c("addsupereff", "sbmsupereff")) {

      t_input <- do.call(rbind, lapply(deasol$DMU, function(x)
        x$t_input))
      t_output <- do.call(rbind, lapply(deasol$DMU, function(x)
        x$t_output))

      slacklist <- list(superslack_input = t_input,
                        superslack_output = t_output,
                        slack_input = slack_input,
                        slack_output = slack_output)

    } else {
      slacklist <- list(slack_input = slack_input,
                        slack_output = slack_output)
    }

    if (is.null(slack_input) && is.null(slack_output) && is.null(t_input) && is.null(t_output)) {
      stop("No slack/superslack parameters in this solution!")
    }

  } else if (is.dea_fuzzy(deasol)) {

    dmunames_eval <- deasol$data$dmunames[deasol$dmu_eval]
    dmunames_ref <- deasol$data$dmunames[deasol$dmu_ref]
    inputnames <- rownames(deasol$data$input$mL)
    outputnames <- rownames(deasol$data$output$mL)
    nde <- length(deasol$dmu_eval)
    ni <- length(deasol$data$input$mL[, 1])
    no <- length(deasol$data$output$mL[, 1])

    if (grepl("kaoliu", deasol$modelname)) {
      nalpha <- length(deasol$alpha)

      slack_input.W <- NULL
      slack_input.B <- NULL
      if ("slack_input" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {

        slack_input.W <- array(0,
                               dim = c(nde, ni, nalpha),
                               dimnames = list(dmunames_eval, inputnames, names(deasol$alphacut)))
        slack_input.B <- slack_input.W

        for (i in 1:nalpha) {
          slack_input.W[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$slack_input))
          slack_input.B[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$slack_input))
        }
      }

      slack_output.W <- NULL
      slack_output.B <- NULL
      if ("slack_output" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {

        slack_output.W <- array(0,
                                dim = c(nde, no, nalpha),
                                dimnames = list(dmunames_eval, outputnames, names(deasol$alphacut)))
        slack_output.B <- slack_output.W

        for (i in 1:nalpha) {
          slack_output.W[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$slack_output))
          slack_output.B[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$slack_output))
        }

      }

      t_input.W <- NULL
      t_output.W <- NULL
      t_input.B <- NULL
      t_output.B <- NULL
      if (grepl("addsupereff", deasol$modelname) || grepl("sbmsupereff", deasol$modelname)) {

        t_input.W <- array(0,
                           dim = c(nde, ni, nalpha),
                           dimnames = list(dmunames_eval, inputnames, names(deasol$alphacut)))
        t_input.B <- t_input.W

        for (i in 1:nalpha) {
          t_input.W[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$t_input))
          t_input.B[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$t_input))
        }
        t_output.W <- array(0,
                            dim = c(nde, no, nalpha),
                            dimnames = list(dmunames_eval, outputnames, names(deasol$alphacut)))
        t_output.B <- t_output.W

        for (i in 1:nalpha) {
          t_output.W[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$t_output))
          t_output.B[, , i] <- do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$t_output))
        }

        slacklist <- list(superslack_input.W = t_input.W,
                          superslack_input.B = t_input.B,
                          superslack_output.W = t_output.W,
                          superslack_output.B = t_output.B,
                          slack_input.W = slack_input.W,
                          slack_input.B = slack_input.B,
                          slack_output.W = slack_output.W,
                          slack_output.B = slack_output.B)

      } else {
        slacklist <- list(slack_input.W = slack_input.W,
                          slack_input.B = slack_input.B,
                          slack_output.W = slack_output.W,
                          slack_output.B = slack_output.B)
      }

      if (is.null(slack_input.W) && is.null(slack_output.W) && is.null(t_input.W) && is.null(t_output.W)) {
        stop("No slack/superslack parameters in this solution!")
      }

    } else if (grepl("possibilistic", deasol$modelname)) {
      nh <- length(deasol$h)

      if (any(grepl("slack", names(deasol$hlevel[[1]]$DMU[[1]])))) {

        slack_input <- NULL
        if ("slack_input" %in% names(deasol$hlevel[[1]]$DMU[[1]])) {

          slack_input <- array(0,
                               dim = c(nde, ni, nh),
                               dimnames = list(dmunames_eval, inputnames, names(deasol$hlevel)))

          for (i in 1:nh) {
            slack_input[, , i] <- do.call(rbind, lapply(deasol$hlevel[[i]]$DMU, function(x)
              x$slack_input))
          }

          slack_output <- array(0,
                                dim = c(nde, no, nh),
                                dimnames = list(dmunames_eval, outputnames, names(deasol$hlevel)))

          for (i in 1:nh) {
            slack_output[, , i] <- do.call(rbind, lapply(deasol$hlevel[[i]]$DMU, function(x)
              x$slack_output))
          }

          slacklist <- list(slack_input = slack_input,
                            slack_output = slack_output)

        }

      } else {
        stop("No slack parameters in this solution!")
      }

    }

  } else {
    stop("Input should be a dea or dea_fuzzy class object!")
  }

  return(slacklist)

}
