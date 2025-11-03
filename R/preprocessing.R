#' @title Validate PEAXAI Input Parameters
#'
#' @description
#' Checks that the arguments supplied to PEAXAI fitting/projection routines are
#' correctly specified. The function validates types, index ranges, and basic
#' domain constraints (e.g., non-overlapping input/output indices, admissible
#' returns-to-scale codes, probability ranges), and throws informative errors
#' when violations are detected.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables used in the model.
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param RTS Returns-to-scale specification. Either a numeric code in
#'   \code{c(0,1,2,3,4,5)} or a character in
#'   \code{c("fdh","vrs","drs","crs","irs","add")}, matching the options in
#'   \code{Benchmarking::dea.add()}.
#' @param trControl A list with resampling/control settings (e.g.,
#'   \code{list(method = "cv", number = 5)} or \code{list(method = "test_set", ...)}).
#'   The field \code{method} must be one of \code{"cv"} or \code{"test_set"}.
#' @param methods Character vector with the learning algorithms to be tried (passed to caret);
#'   not validated here but kept for completeness.
#' @param metric_priority Character string with the primary performance metric name (e.g., \code{"ROC"} or \code{"Accuracy"});
#'   not validated here but kept for completeness.
#' @param hold_out Numeric proportion in (0,1) used when \code{trControl$method == "test_set"};
#'   not validated here but kept for completeness.
#' @param imbalance_rate Optional numeric vector with class proportions or target imbalance
#'   rates. All values must be strictly between 0 and 1 if provided.
#' @param verbose Logical; whether to print additional diagnostic messages (not used in this checker).
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item \strong{Data container}: \code{data} must be a non-empty \code{data.frame} or \code{matrix}.
#'   \item \strong{Index vectors}: \code{x} and \code{y} must be numeric indices within \code{ncol(data)} and must not overlap.
#'   \item \strong{RTS}: must be either one of the numeric codes \code{0:5} or one of
#'         \code{"fdh","vrs","drs","crs","irs","add"} (as in \code{Benchmarking::dea.add}).
#'   \item \strong{Resampling}: \code{trControl$method} must be \code{"cv"} or \code{"test_set"}.
#'   \item \strong{Imbalance}: if \code{imbalance_rate} is provided, all entries must
#'         lie in the open interval \code{(0,1)}.
#' }
#' Any violation triggers \code{stop()} with a descriptive message.
#'
#' @return
#' Invisibly returns \code{NULL} on success (i.e., when all checks pass). Otherwise,
#' throws an error describing the first problem encountered.
#'
#' @seealso
#' \code{\link[Benchmarking]{dea.add}} for RTS codes; \code{\link[caret]{trainControl}}
#' for resampling specifications used during model training.
#'
#' @examples
#' \dontrun{
#' data(firms)
#' validate_parametes(
#'   data = firms,
#'   x = 1:4, y = 5,
#'   RTS = "vrs",
#'   trControl = list(method = "cv", number = 5),
#'   methods = c("nnet","svmPoly"),
#'   metric_priority = "ROC",
#'   hold_out = 0.2,
#'   imbalance_rate = seq(0.1, 0.5, 0.1),
#'   verbose = FALSE
#' )
#' }

# check if parameters are well introduced
validate_parametes <- function(
  # PEAXAI_fitting
  data, x, y, RTS, trControl, methods, metric_priority,
  hold_out, imbalance_rate, verbose

) {

  # ----------------------------------------------------------------------------
  # data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (is.null(data)) {
    stop("'data' must be a data.frame or matrix; got NULL.", call. = FALSE)
  }

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }

  if (nrow(data) == 0L || nrow(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) || is.numeric(y))) {

    stop("The arguments 'x' and 'y' must be vectors containing the indexes of the input and output variables, respectively.", call. = FALSE)

  } else {

    # same index or names
    if(any(x %in% y)) {
      stop("There are one or more variables that are included as inputs and outputs at the same time. The arguments 'x' and 'y' must not overlap.", call. = FALSE)
    }

  }

  # ----------------------------------------------------------------------------
  # RTS ------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  RTS_available_num <- c(0,1,2,3,4,5)
  RTS_available_char <- c("fdh", "vrs", "drs", "crs", "irs", "add")

  if (is.numeric(RTS)) {
    if(!RTS %in% RTS_available_num) {
      stop("The argument 'RTS' has to be one of the RTS available on the function 'dea.add' from Benchmarking package.", call. = FALSE)
    }
  } else if (is.character(RTS)) {
    if(!RTS %in% RTS_available_char) {
      stop("The argument 'RTS' has to be one of the RTS available on the function 'dea.add' from Benchmarking package.", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # trControl ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  browser()
  if (is.null(trControl) || (is.list(trControl) && length(trControl) == 0L)) {
    stop("'trControl' must be specified (e.g., list(method = 'cv', number = 5)).", call. = FALSE)
  }

  method_available <- c("cv")

  if (!is.character(trControl[["method"]]) || length(trControl[["method"]]) != 1L || !(trControl[["method"]] %in% method_available)) {
    stop(
      paste0(
        "`trControl$method` must be one of  these methods: ", method_available),
      call. = FALSE
    )
  }



  # ----------------------------------------------------------------------------
  # imbalance_rate -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.null(imbalance_rate)) {

    if (!is.numeric(imbalance_rate)) {
      stop(
        "'imbalance_rate' must be a numeric vector.",
        call. = FALSE
      )
    }

    if (any(imbalance_rate <= 0 | imbalance_rate >= 1)) {
      stop(
        "All values in 'imbalance_rate' must be strictly between 0 and 1. (e.g., seq(0.1, 0.5, 0.1)",
        call. = FALSE
      )
    }

  }

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
