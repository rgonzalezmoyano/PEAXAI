#' @title Handle Errors on PEAXAI_fitting
#'
#' @description This function check if PEAXAI arguments are well introduced and check if the analysis it will be done.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#'
#' @importFrom caret trainControl
#'
#' @return It returns a \code{matrix} in the required format and displays some error messages.

# check if parameters are well introduced
validate_fitting_args <- function (
  data, x, y, RTS, trControl, methods,
  metric, importance_method, hold_out,
  balance_data, scenarios
  ) {

  # data
  if (!is.data.frame(data)) {
    stop("The argument 'data' has to be a dataframe or matrix.")
  }

  # x and y
  if (!(is.vector(x) || is.vector(y))) {

    stop("The arguments 'x' and 'y' must be vectors containing the indexes or names of the input and output variables, respectively.")

  } else {

    # same index or names
    if(any(x %in% y)) {
      stop("There are one or more variables that are included as inputs and outputs at the same time. The arguments 'x' and 'y' must not overlap.")
    }


  }

  # RTS
  RTS_available <- c(
    0, "fdh",
    1, "vrs",
    2, "drs",
    3, "crs",
    4, "irs",
    5, " add"
  )

  if(!RTS %in% RTS_available) {
    stop("The argument 'RTS' has to be one of the RTS available on the function 'dea.add' from Benchmarking package.")
  }

  # trControl
  if(!class(trControl) == "list") {
    stop("The argument 'trControl' has to be an object type 'list'. The list has to indicate how the hyperparameters will be chosen.")
  } else {

    # no cross-validation
    if(trControl[["method"]] == "none") {

      # no test set
      if(!is.null(trControl[["test_hold_out"]])) {
        warning("Due to the 'method' is 'none', the value 'test_hold_out' in 'trControl' will be ignored (treated as NULL).")
      }

      warning("No test set is being used to select/tune the best hyperparameters. ",
              "Model performance will be estimated on the training/validation data only, ",
              "which increases the risk of overfitting.")


    } else if (trControl[["method"]] == "test_set") {

      # no test set
      if(is.null(trControl[["test_hold_out"]])) {
        stop("If 'method' is 'test_hold_out', you must specify the proportion of the test set.")
      }


    } else if (trControl[["method"]] == "cv") {

    }
  }


  # methods

  # metric

  # importance_method

  # hold_out

  # balance_data

  # scenarios
  # print("end")
}








#' @title Prepare Data and Handle Errors
#'
#' @description This function arranges the data in the required format and displays some error messages.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#'
#' @importFrom caret trainControl
#'
#' @return It returns a \code{matrix} in the required format and displays some error messages.

preprocessing <- function (
    data, x, y
    ) {

  # x and y well / bad introduced

  cols <- 1:length(data)
  if (!(all(x %in% cols))) {
    stop("x index(es) are not in data.")

    if (!(all(y %in% cols))) {
      stop("y index(es) are not in data.")
    }
  }

  # (i) data.frame, (ii) list with variables, (iii) matrix

  # data.frame format to deal with classes
  if (is.list(data) && !is.data.frame(data)) {

    # data names?
    ifelse(is.null(names(data)),
           var_names <- 1:length(data), # if not 1:x
           var_names <- names(data)
           )

    data <- matrix(unlist(data), ncol = length(var_names), byrow = F)
    colnames(data) <- var_names

  } else if (is.matrix(data) || is.data.frame(data)) {
    data <- data.frame(data)
  }

  # Classes
  varClass <- unlist(sapply(data, class))

  # Output classes
  outClass <- varClass[y] %in% c("numeric", "double", "integer")

  # Error
  if (!all(outClass)){
    stop(paste(names(data)[y][!outClass][1], "is not a numeric or integer vector"))
  }

  # Input classes
  # Ordered --> numeric
  for (i in x){
    if (is.ordered(data[, i])) {
      data[, i] <- as.numeric(data[, i])
    }
  }

  # Define classes again
  varClass <- unlist(sapply(data, class))

  inpClass <- varClass[x] %in% c("numeric", "double", "integer")

  # Error
  if (!all(inpClass)){
    stop(paste(names(data)[x][!inpClass][1], "is not a numeric, integer or ordered vector"))
  }

  data <- data[, c(x, y)]

  return(as.matrix(data))
}
