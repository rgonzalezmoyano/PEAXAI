#' @title Data envelopment analysis' clasification.
#'
#' @description This function trains for each model, the different hyperparameters and returns the best model with its best hyperparameters.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' 
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint get.objective
#'
#' @return Fill
compute_scores_additive <- function (
    data, x, y
    ) {
  
  # number of inputs
  nX <- length(x)
  
  # number of outputs
  nY <- length(y)
    
  # matrix of inputs 
  xmat <- as.matrix(data[, x])
  
  # matrix of outputs
  ymat <- as.matrix(data[, y])
    
  # number of dmus
  dmu <- nrow(data)
  
  # initialize vectors of scores
  scores <- matrix(nrow = dmu, ncol = 1) 
    
  for (d in 1:dmu) {
      
    # vector for variables: slack_X + slack_y + lambdas
    objVal <- matrix(ncol = nX + nY + dmu, nrow = 1) 
      
    objVal[1:(nX + nY)] <- 1
      
    lps <- make.lp(nrow = 0, ncol = nX + nY + dmu)
    lp.control(lps, sense = "max")
    set.objfn(lps, objVal)
      
    for(xi in 1:nX) {
        
      # slacks for inputs
      x_slack <- rep(0, nX)
      x_slack[xi] <- 1
      slacks  <- c(x_slack, rep(0, nY))
        
      add.constraint(lps, xt = c(slacks, xmat[, xi]), "=", rhs = xmat[d, xi])
      
    }
      
    for(yi in 1:nY) {
        
      # Slacks para outputs
      y_slack <- rep(0, nY)
      y_slack[yi] <- - 1
      slacks  <- c(rep(0, nX), y_slack)
        
      add.constraint(lps, xt = c(slacks, ymat[,yi]), "=", rhs = ymat[d, yi])
        
    }
      
    # convexitiy and variable returns to scale
    add.constraint(lprec = lps, xt = c(rep(0, nX + nY), rep(1, dmu)), type = "=", rhs = 1)
      
    solve(lps)
    scores[d, ] <- get.objective(lps)
      
  }

  return(scores)
}

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
