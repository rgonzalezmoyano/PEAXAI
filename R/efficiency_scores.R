#' @title The output-oriented radial model in the envelopment format
#'
#' @description This function computes the efficiency scores through the output-oriented radial model in the envelopment format.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective get.variables
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the output-oriented radial model in the envelopment format.

rad_out <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  for (d in 1:eval_dmu) {

    objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
    objVal[1] <- 1

    lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)

    # inputs
    for (xi in 1:nX) {
      add.constraint(lps, xt = c(0, tech_xmat[, xi]), "<=",  rhs = eval_xmat[d, xi])
    }

    # outputs
    for (yi in 1:nY) {
      add.constraint(lps, xt = c(- eval_ymat[d, yi], tech_ymat[, yi]), ">=", rhs = 0)
    }

    # technology
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}

#' @title The input-oriented radial model in the envelopment format
#'
#' @description This function computes efficiency scores through the input-oriented radial model in the envelopment format.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the input-oriented radial model in the envelopment format.

rad_inp <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  for (d in 1:eval_dmu) {

    objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
    objVal[1] <- 1

    lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)

    # inputs
    for (xi in 1:nX) {
      add.constraint(lps, xt = c(- eval_xmat[d, xi], tech_xmat[, xi]), "<=",  rhs = 0)
    }

    # outputs
    for (yi in 1:nY) {
      add.constraint(lps, xt = c(0, tech_ymat[, yi]), ">=", rhs = eval_ymat[d, yi])
    }

    # technology
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}

#' @title The Directional Distance Function
#'
#' @description This function computes efficiency scores through the directional distance function in the envelopment format.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param direction Direction of the vector to project on the frontier. Two possibilities:
#' \itemize{
#' \item{\code{"mean"}} Projection vector given by the average value of inputs and outputs of all DMUs.
#' \item{\code{"briec"}} Projection vector given by the value of inputs and outputs of the evaluated DMU.
#' }
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the input-oriented radial model in the envelopment format.

ddf <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, direction, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  for (d in 1:eval_dmu) {
    objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
    objVal[1] <- 1

    # structure for lpSolve
    lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)

    if (direction == "mean") {
      G_x <- colMeans(tech_xmat)
      G_y <- colMeans(tech_ymat)
    } else {
      G_x <- matrix(eval_xmat[d, ], nrow = 1)
      G_y <- matrix(eval_ymat[d, ], nrow = 1)
    }

    # inputs
    for (xi in 1:nX) {
      add.constraint(lps, xt = c(G_x[, xi], tech_xmat[, xi]), "<=",  rhs = eval_xmat[d, xi])
    }

    # outputs
    for (yi in 1:nY) {
      add.constraint(lps, xt = c(- G_y[, yi], tech_ymat[, yi]), ">=", rhs =  eval_ymat[d, yi])
    }

    # technology
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}

#' @title The output-oriented Russell model in the envelopment format
#'
#' @description This function computes efficiency scores through the output-oriented Russell model in the envelopment format.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the output-oriented Russell model.

rsl_out <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  for (d in 1:eval_dmu) {

    objVal <- matrix(ncol = nY + tech_dmu, nrow = 1)
    objVal[1:nY] <- 1 / nY

    # structure for lpSolve
    lps <- make.lp(nrow = 0, ncol = tech_dmu + nY)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)

    # inputs
    for (xi in 1:nX) {
      add.constraint(lps, xt = c(rep(0, nY), tech_xmat[, xi]), "<=",  rhs = eval_xmat[d, xi])
    }

    # outputs
    for (yi in 1:nY) {
      phi <- rep(0, nY)
      phi[yi] <- - eval_ymat[d, yi]
      add.constraint(lps, xt = c(phi, tech_ymat[, yi]), ">=", rhs = 0)
    }

    # lower bounds: phi >= 1
    phi.idx <- 1:nY
    set.bounds(lps, lower = rep(1, nY), upper = rep(Inf, nY), columns = phi.idx)

    # technology
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}

#' @title The input-oriented Russell model in the envelopment format
#'
#' @description This function computes efficiency scores through the input-oriented Russell model in the envelopment format.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the input-oriented Russell model.

rsl_inp <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  for (d in 1:eval_dmu) {

    objVal <- matrix(ncol = nX + tech_dmu, nrow = 1)
    objVal[1:nX] <- 1 / nX

    # structure for lpSolve
    lps <- make.lp(nrow = 0, ncol = nX + tech_dmu)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)

    # inputs
    for (xi in 1:nX) {
      the <- rep(0, nX)
      the[xi] <- - eval_xmat[d, xi]
      add.constraint(lps, xt = c(the, tech_xmat[, yi]), "<=",  rhs = 0)
    }

    # outputs
    for (yi in 1:nY) {
      add.constraint(lps, xt = c(rep(0, nX), tech_xmat[, yi]), ">=", rhs = ymat[d, yi])
    }

    # upper bounds: theta <= 1
    the.idx <- 1:nX
    set.bounds(lps, lower = rep(0, nX), upper = rep(1, nX), columns = the.idx)

    # technology
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}

#' @title The Weighted Additive Model
#'
#' @description This function computes efficiency scores through a Weighted Additive Model.
#'
#' @param tech_xmat A \code{data.frame} or \code{matrix} containing the observed inputs to determine the technology.
#' @param tech_ymat A \code{data.frame} or \code{matrix} containing the observed outputs to determine the technology.
#' @param eval_xmat A \code{data.frame} or \code{matrix} containing the containing the input data of the DMUs to be evaluated.
#' @param eval_ymat A \code{data.frame} or \code{matrix} containing the containing the output data of the DMUs to be evaluated.
#' @param weights Weights for the additive model:
#' \itemize{
#' \item{\code{"WAM"}} Weighted Additive Model.
#' \item{\code{"MIP"}} Measure of Inefficiency Proportions.
#' \item{\code{"NOR"}} Normalized Weighted Additive Model.
#' \item{\code{"RAM"}} Range Adjusted Measure.
#' \item{\code{"BAM"}} Bounded Adjusted Measure.
#' }
#' @param convexity \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A \code{vector} of \code{"numeric"} scores computed through the Weighted Additive Model.

wam <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, weights, convexity, returns
    ) {

  # number of DMUs in the technology
  tech_dmu <- nrow(tech_xmat)

  # number of DMUs to be evaluated
  eval_dmu <- nrow(eval_xmat)

  # initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)

  # number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)

  scores <- matrix(nrow = eval_dmu, ncol = 1)

  for (d in 1:eval_dmu) {

    # objective function
    objVal <- matrix(ncol = nX + nY + tech_dmu, nrow = 1)

    # Weights
    if (weights == "WAM") {
      # Weighted Additive Model
      objVal[1:(nX + nY)] <- 1

    } else if (weights == "MIP") {
      # Measure of Inefficiency Proportions
      objVal[1:(nX + nY)] <- c(1 / xmat[d, ], 1 / ymat[d, ])

    } else if (weights == "NOR") {
      # Normalized Weighted Additive Model
      objVal[1:(nX + nY)] <- c(1 / apply(xmat, 2, sd), 1 / apply(ymat, 2, sd))

    } else if (weights == "RAM") {
      # Range Adjusted Measure
      xranges <- apply(xmat, 2, max) - apply(xmat, 2, min)
      yranges <- apply(ymat, 2, max) - apply(ymat, 2, min)
      objVal[1:(nX + nY)] <- c(1 / ((nX + nY) * xranges), 1 / ((nX + nY) * yranges))

    } else if (weights == "BAM") {
      # Bounded Adjusted Measure
      p1 <- xmat[d, ] - apply(xmat, 2, min)
      p2 <- apply(ymat, 2, max) - ymat[d, ]
      objVal[1:(nX + nY)] <- c(1 / ((nX + nY) * p1), 1 / ((nX + nY) * p2))

    } else {
      stop(print(paste(weights, "no disponibles")))
    }

    # structure for lpSolve
    lps <- make.lp(nrow = 0, ncol = nX + nY + tech_dmu)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)

    # inputs
    for (xi in 1:nX) {
      x_slack <- rep(0, nX)
      x_slack[xi] <- 1
      slacks <- c(x_slack, rep(0, nY))

      add.constraint(lps, xt = c(slacks, tech_xmat[, xi]), "=", rhs = eval_dmu[d, xi])
    }

    # outputs
    for (yi in 1:nY) {
      y_slack <- rep(0, nY)
      y_slack[yi] <- - 1
      slacks <- c(rep(0, nX), y_slack)

      add.constraint(lps, xt = c(slacks, tech_ymat[, yi]), "=", rhs = eval_dmu[d, yi])
    }

    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(rep(0, nX + nY), rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(rep(0, nX + nY), rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + (nX + nY), type = c("binary"))
      }
    }

    solve(lps)
    scores[d, ] <- get.objective(lps)
  }

  return(scores)
}