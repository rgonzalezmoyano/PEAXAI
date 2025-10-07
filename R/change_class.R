#' @title Change Column Classes in a Data Frame
#'
#' @description This function changes the class of specified columns in a data frame to one of several types: numeric, integer, character, factor, or logical. By default, column indexes are expected. If column names are provided, they will be converted to their corresponding indexes.
#'
#' @param data A \code{data.frame} that contains the variables whose types are to be changed.
#' @param to_numeric Column indexes (or names) of variables in \code{data} to be converted to \code{numeric}.
#' @param to_integer Column indexes (or names) of variables in \code{data} to be converted to \code{integer}.
#' @param to_character Column indexes (or names) of variables in \code{data} to be converted to \code{character}.
#' @param to_factor Column indexes (or names) of variables in \code{data} to be converted to \code{factor}.
#' @param to_logical Column indexes (or names) of variables in \code{data} to be converted to \code{logical}.
#'
#' @details The function converts the specified columns to the desired class. By default, column indexes should be provided as integers. If column names are passed, they will be automatically converted to their corresponding column indexes. The function will stop if there are duplicated column references across the different conversion categories.
#'
#' @return A \code{data.frame} with the specified columns converted to the desired class types.
#'
#' @export

change_class <- function (data, to_numeric = NULL, to_integer = NULL, to_character = NULL, to_factor = NULL, to_logical = NULL) {
  
  # Verify if the dataset is a data.frame
  if (!is.data.frame(data)) {
    stop("The input data is not a data.frame")
  }
  
  # Convert column names to indices for to_factor
  if (!is.null(to_factor) && !is.numeric(to_factor)) {
    to_factor <- match(to_factor, names(data))
  }
  
  # Convert column names to indices for to_numeric
  if (!is.null(to_numeric) && !is.numeric(to_numeric)) {
    to_numeric <- match(to_numeric, names(data))
  }
  
  # Convert column names to indices for to_integer
  if (!is.null(to_integer) && !is.numeric(to_integer)) {
    to_integer <- match(to_integer, names(data))
  }
  
  # Convert column names to indices for to_character
  if (!is.null(to_character) && !is.numeric(to_character)) {
    to_character <- match(to_character, names(data))
  }
  
  # Convert column names to indices for to_logical
  if (!is.null(to_logical) && !is.numeric(to_logical)) {
    to_logical <- match(to_logical, names(data))
  }
  
  # Check for duplicated columns across the lists
  all_cols <- c(to_factor, to_numeric, to_integer, to_character, to_logical)
  if (any(duplicated(all_cols))) {
    stop("Some columns are duplicated across the conversion categories.")
  }
  
  # Apply the conversions
  if (!is.null(to_factor)) {
    data[to_factor] <- lapply(data[to_factor], as.factor)
  }
  
  if (!is.null(to_numeric)) {
    data[to_numeric] <- lapply(data[to_numeric], as.numeric)
  }
  
  if (!is.null(to_integer)) {
    data[to_integer] <- lapply(data[to_integer], as.integer)
  }
  
  if (!is.null(to_character)) {
    data[to_character] <- lapply(data[to_character], as.character)
  }
  
  if (!is.null(to_logical)) {
    data[to_logical] <- lapply(data[to_logical], as.logical)
  }
  
  return(data)
}
