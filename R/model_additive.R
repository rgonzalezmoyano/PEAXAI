#' @title Additive DEA model.
#'
#' @description Solve the additive model of Charnes et. al (1985). With the current
#' version of deaR, it is possible to solve input-oriented, output-oriented,
#' and non-oriented additive model under constant and non-constant returns to scale.
#'
#' Besides, the user can set weights for the input slacks and/or output slacks. So,
#' it is also possible to solve weighted additive models. For example: Measure of
#' Inefficiency Proportions (MIP), Range Adjusted Measure (RAM), etc.
#'
#' @note In this model, the efficiency score is the sum of the slacks. Therefore,
#' a DMU is efficient when the objective value (\code{objval}) is zero.
#'
#' @usage model_additive(datadea,
#'                dmu_eval = NULL,
#'                dmu_ref = NULL,
#'                orientation = NULL,
#'                weight_slack_i = 1,
#'                weight_slack_o = 1,
#'                rts = c("crs", "vrs", "nirs", "ndrs", "grs"),
#'                L = 1,
#'                U = 1,
#'                compute_target = TRUE,
#'                returnlp = FALSE,
#'                ...)
#'
#' @param datadea A \code{deadata} object with \code{n} DMUs, \code{m} inputs and \code{s} outputs.
#' @param dmu_eval A numeric vector containing which DMUs have to be evaluated.
#' If \code{NULL} (default), all DMUs are considered.
#' @param dmu_ref A numeric vector containing which DMUs are the evaluation reference set.
#' If \code{NULL} (default), all DMUs are considered.
#' @param orientation This parameter is either \code{NULL} (default) or a string, equal to
#' "io" (input-oriented) or "oo" (output-oriented). It is used to modify the weight slacks.
#' If input-oriented, \code{weight_slack_o} are taken 0.
#' If output-oriented, \code{weight_slack_i} are taken 0.
#' @param weight_slack_i A value, vector of length \code{m}, or matrix \code{m} x
#' \code{ne} (where \code{ne} is the length of \code{dmu_eval})
#' with the weights of the input slacks. If 0, output-oriented.
#' @param weight_slack_o A value, vector of length \code{s}, or matrix \code{s} x
#' \code{ne} (where \code{ne} is the length of \code{dmu_eval})
#' with the weights of the output slacks. If 0, input-oriented.
#' @param rts A string, determining the type of returns to scale, equal to "crs" (constant),
#' "vrs" (variable), "nirs" (non-increasing), "ndrs" (non-decreasing) or "grs" (generalized).
#' @param L Lower bound for the generalized returns to scale (grs).
#' @param U Upper bound for the generalized returns to scale (grs).
#' @param compute_target Logical. If it is \code{TRUE}, it computes targets.
#' We note that we call "targets" to the "efficient projections"
#' in the strongly efficient frontier.
#' @param returnlp Logical. If it is \code{TRUE}, it returns the linear problems
#' (objective function and constraints).
#' @param ... Ignored, for compatibility issues.
#'
#' @returns A list of class \code{dea} with the results for the evaluated DMUs (\code{DMU} component),
#'  along with any other necessary information to replicate the results, such as
#'  the name of the model and parameters \code{orientation}, \code{rts},
#'  \code{dmu_eval} and \code{dmu_ref}.
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
#' Charnes, A.; Cooper, W.W.; Golany, B.; Seiford, L.; Stuz, J. (1985) "Foundations
#' of Data Envelopment Analysis for Pareto-Koopmans Efficient Empirical Production
#' Functions", Journal of Econometrics, 30(1-2), 91-107.
#' \doi{10.1016/0304-4076(85)90133-2}
#'
#' Charnes, A.; Cooper, W.W.; Lewin, A.Y.; Seiford, L.M. (1994). Data Envelopment
#' Analysis: Theory, Methology, and Application. Boston: Kluwer Academic Publishers.
#' \doi{10.1007/978-94-011-0637-5}
#'
#' Cooper, W.W.; Park, K.S.; Pastor, J.T. (1999). "RAM: A Range Adjusted Measure
#' of Inefficiencies for Use with Additive Models, and Relations to Other Models
#' and Measures in DEA". Journal of Productivity Analysis, 11, p. 5-42.
#' \doi{10.1023/A:1007701304281}
#'
#' @examples
#' # Example 1.
#' # Replication of results in Charnes et. al (1994, p. 27)
#' x <- c(2, 3, 6, 9, 5, 4, 10)
#' y <- c(2, 5, 7, 8, 3, 1, 7)
#' data_example <- data.frame(dmus = letters[1:7], x, y)
#' data_example <- make_deadata(data_example,
#'                              ni = 1,
#'                              no = 1)
#' result <- model_additive(data_example,
#'                          rts = "vrs")
#' efficiencies(result)
#' slacks(result)
#' lambdas(result)
#'
#' # Example 2.
#' # Measure of Inefficiency Proportions (MIP).
#' x <- c(2, 3, 6, 9, 5, 4, 10)
#' y <- c(2, 5, 7, 8, 3, 1, 7)
#' data_example <- data.frame(dmus = letters[1:7], x, y)
#' data_example <- make_deadata(data_example,
#'                              ni = 1,
#'                              no = 1)
#' result2 <- model_additive(data_example,
#'                           rts = "vrs",
#'                           weight_slack_i = 1 / data_example[["input"]],
#'                           weight_slack_o = 1 / data_example[["output"]])
#' slacks(result2)
#'
#' # Example 3.
#' # Range Adjusted Measure of Inefficiencies (RAM).
#' x <- c(2, 3, 6, 9, 5, 4, 10)
#' y <- c(2, 5, 7, 8, 3, 1, 7)
#' data_example <- data.frame(dmus = letters[1:7], x, y)
#' data_example <- make_deadata(data_example,
#'                              ni = 1,
#'                              no = 1)
#' range_i <- apply(data_example[["input"]], 1, max) -
#'            apply(data_example[["input"]], 1, min)
#' range_o <- apply(data_example[["output"]], 1, max) -
#'            apply(data_example[["output"]], 1, min)
#' w_range_i <- 1 / (range_i * (dim(data_example[["input"]])[1] +
#'                              dim(data_example[["output"]])[1]))
#' w_range_o <- 1 / (range_o * (dim(data_example[["input"]])[1] +
#'                              dim(data_example[["output"]])[1]))
#' result3 <- model_additive(data_example,
#'                           rts = "vrs",
#'                           weight_slack_i = w_range_i,
#'                           weight_slack_o = w_range_o)
#' slacks(result3)
#'
#' @seealso \code{\link{model_addsupereff}}
#'
#' @import lpSolve
#'
#' @export

model_additive <-
  function(datadea,
           dmu_eval = NULL,
           dmu_ref = NULL,
           orientation = NULL,
           weight_slack_i = 1,
           weight_slack_o = 1,
           rts = c("crs", "vrs", "nirs", "ndrs", "grs"),
           L = 1,
           U = 1,
           compute_target = TRUE,
           returnlp = FALSE,
           ...) {

    # Cheking whether datadea is of class "deadata" or not...
    if (!is.deadata(datadea)) {
      stop("Data should be of class deadata. Run make_deadata function first!")
    }

    # Checking non-discretionary inputs/outputs
    if ((!is.null(datadea$nd_inputs)) || (!is.null(datadea$nd_outputs))) {
      warning("This model does not take into account the non-discretionary feature for inputs/outputs.")
    }

    # Checking undesirable inputs/outputs
    if (!is.null(datadea$ud_inputs) || !is.null(datadea$ud_outputs)) {
      warning("This model does not take into account the undesirable feature for inputs/outputs.")
    }

    # Checking rts
    rts <- tolower(rts)
    rts <- match.arg(rts)

    if (rts == "grs") {
      if (L > 1) {
        stop("L must be <= 1.")
      }
      if (U < 1) {
        stop("U must be >= 1.")
      }
    }

    dmunames <- datadea$dmunames
    nd <- length(dmunames) # number of dmus

    if (is.null(dmu_eval)) {
      dmu_eval <- 1:nd
    } else if (!all(dmu_eval %in% (1:nd))) {
      stop("Invalid set of DMUs to be evaluated (dmu_eval).")
    }
    names(dmu_eval) <- dmunames[dmu_eval]
    nde <- length(dmu_eval)

    if (is.null(dmu_ref)) {
      dmu_ref <- 1:nd
    } else if (!all(dmu_ref %in% (1:nd))) {
      stop("Invalid set of reference DMUs (dmu_ref).")
    }
    names(dmu_ref) <- dmunames[dmu_ref]
    ndr <- length(dmu_ref)

    input <- datadea$input
    output <- datadea$output
    inputnames <- rownames(input)
    outputnames <- rownames(output)
    ni <- nrow(input) # number of  inputs
    no <- nrow(output) # number of outputs
    inputref <- matrix(input[, dmu_ref], nrow = ni)
    outputref <- matrix(output[, dmu_ref], nrow = no)

    nc_inputs <- datadea$nc_inputs
    nc_outputs <- datadea$nc_outputs
    nnci <- length(nc_inputs)
    nnco <- length(nc_outputs)

    # Checking weights
    if(is.null(weight_slack_i)){
      weight_slack_i <- 1
    }
    if(is.null(weight_slack_o)){
      weight_slack_o <- 1
    }

    if (is.matrix(weight_slack_i)) {
      if ((nrow(weight_slack_i) != ni) || (ncol(weight_slack_i) != nde)) {
        stop("Invalid weight input matrix (number of inputs x number of evaluated DMUs).")
      }
    } else if ((length(weight_slack_i) == 1) || (length(weight_slack_i) == ni)) {
      weight_slack_i <- matrix(weight_slack_i, nrow = ni, ncol = nde)
    } else {
      stop("Invalid weight input vector (number of inputs).")
    }
    if ((!is.null(orientation)) && (orientation == "oo")) {
      weight_slack_i <- matrix(0, nrow = ni, ncol = nde)
    }
    rownames(weight_slack_i) <- inputnames
    colnames(weight_slack_i) <- dmunames[dmu_eval]

    if (is.matrix(weight_slack_o)) {
      if ((nrow(weight_slack_o) != no) || (ncol(weight_slack_o) != nde)) {
        stop("Invalid weight output matrix (number of outputs x number of evaluated DMUs).")
      }
    } else if ((length(weight_slack_o) == 1) || (length(weight_slack_o) == no)) {
      weight_slack_o <- matrix(weight_slack_o, nrow = no, ncol = nde)
    } else {
      stop("Invalid weight output vector (number of outputs).")
    }
    if ((!is.null(orientation)) && (orientation == "io")) {
      weight_slack_o <- matrix(0, nrow = no, ncol = nde)
    }
    rownames(weight_slack_o) <- outputnames
    colnames(weight_slack_o) <- dmunames[dmu_eval]

    target_input <- NULL
    target_output <- NULL

    DMU <- vector(mode = "list", length = nde)
    names(DMU) <- dmunames[dmu_eval]

    ###########################

    if (rts == "crs") {
      f.con.rs <- NULL
      f.dir.rs <- NULL
      f.rhs.rs <- NULL
    } else {
      f.con.rs <- cbind(matrix(1, nrow = 1, ncol = ndr), matrix(0, nrow = 1, ncol = ni + no))
      if (rts == "vrs") {
        f.dir.rs <- "="
        f.rhs.rs <- 1
      } else if (rts == "nirs") {
        f.dir.rs <- "<="
        f.rhs.rs <- 1
      } else if (rts == "ndrs") {
        f.dir.rs <- ">="
        f.rhs.rs <- 1
      } else {
        f.con.rs <- rbind(f.con.rs, f.con.rs)
        f.dir.rs <- c(">=", "<=")
        f.rhs.rs <- c(L, U)
      }
    }

    # Constraints matrix
    f.con.1 <- cbind(inputref, diag(ni), matrix(0, nrow = ni, ncol = no))
    f.con.2 <- cbind(outputref, matrix(0, nrow = no, ncol = ni), -diag(no))
    f.con.nc <- matrix(0, nrow = (nnci + nnco), ncol = (ndr + ni + no))
    f.con.nc[, ndr + c(nc_inputs, ni + nc_outputs)] <- diag(nnci + nnco)
    f.con <- rbind(f.con.1, f.con.2, f.con.nc, f.con.rs)

    # Directions vector
    f.dir <- c(rep("=", ni + no + nnci + nnco), f.dir.rs)

    for (i in 1:nde) {

      ii <- dmu_eval[i]

      # Objective function coefficients
      f.obj <- c(rep(0, ndr), weight_slack_i[, i], weight_slack_o[, i])

      # Right hand side vector
      f.rhs <- c(input[, ii], output[, ii], rep(0, nnci + nnco), f.rhs.rs)

      if (returnlp) {

        lambda <- rep(0, ndr)
        names(lambda) <- dmunames[dmu_ref]
        slack_input <- rep(0, ni)
        names(slack_input) <- inputnames
        slack_output <- rep(0, no)
        names(slack_output) <- outputnames
        var <- list(lambda = lambda, slack_input = slack_input, slack_output = slack_output)
        DMU[[i]] <- list(direction = "max", objective.in = f.obj, const.mat = f.con,
                         const.dir = f.dir, const.rhs = f.rhs, var = var)

      } else {

        res <- lp("max", f.obj, f.con, f.dir, f.rhs)

        if (res$status == 0) {

          objval <- res$objval

          lambda <- res$solution[1 : ndr]
          names(lambda) <- dmunames[dmu_ref]

          slack_input <- res$solution[(ndr + 1) : (ndr + ni)]
          names(slack_input) <- inputnames
          slack_output <- res$solution[(ndr + ni + 1) : (ndr + ni + no)]
          names(slack_output) <- outputnames

          if (compute_target) {
            target_input <- as.vector(inputref %*% lambda)
            target_output <- as.vector(outputref %*% lambda)
            names(target_input) <- inputnames
            names(target_output) <- outputnames
          }

        } else {

          objval <- NA
          lambda <- NA
          slack_input <- NA
          slack_output <- NA
          if (compute_target) {
            target_input <- NA
            target_output <- NA
          }

        }

        DMU[[i]] <- list(objval = objval,
                         lambda = lambda,
                         slack_input = slack_input, slack_output = slack_output,
                         target_input = target_input, target_output = target_output)

      }

    }

    # Checking if a DMU is in its own reference set (when rts = "grs")
    if (rts == "grs") {
      eps <- 1e-6
      for (i in 1:nde) {
        j <- which(dmu_ref == dmu_eval[i])
        if (length(j) == 1) {
          kk <- DMU[[i]]$lambda[j]
          kk2 <- sum(DMU[[i]]$lambda[-j])
          if ((kk > eps) && (kk2 > eps)) {
            warning(paste("Under generalized returns to scale,", dmunames[dmu_eval[i]],
                          "appears in its own reference set."))
          }
        }
      }
    }

    deaOutput <- list(modelname = "additive",
                      rts = rts,
                      L = L,
                      U = U,
                      DMU = DMU,
                      data = datadea,
                      dmu_eval = dmu_eval,
                      dmu_ref = dmu_ref,
                      weight_slack_i = weight_slack_i,
                      weight_slack_o = weight_slack_o,
                      orientation = NA)

    return(structure(deaOutput, class = "dea"))

  }
