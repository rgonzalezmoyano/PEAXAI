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
