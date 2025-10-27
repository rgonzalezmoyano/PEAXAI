#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param method Parameters for controlling the training process (from the \code{'caret'} package).
#' @param arguments A \code{list} of selected machine learning models and their hyperparameters.
#' @param hyparameter A \code{list}
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.

#'
#' @importFrom caret train createFolds

#'
#' @return It returns a \code{list} with the chosen model.

xai_prepare_sets <- function (
    data, x, y, final_model, background, target,
    type, threshold, levels_order
    ) {

  # domain set; by default is "train"
  if (background == "train") {

    # a ML model train by caret
    if (class(final_model)[1] == "train") {

      # save train data, change name and position
      train_data <- final_model[["trainingData"]]
      names(train_data)[1] <- "class_efficiency"

      train_data <- train_data[,c(2:length(train_data),1)]

    } else {
      train_data <- final_model[["data"]]
      train_data$class_efficiency <- ifelse(train_data$class_efficiency == 1, "efficient", "not_efficient")
      train_data$class_efficiency <- factor(
        train_data$class_efficiency,
        levels = levels_order
      )

    }

  } else if (background == "real") {

    n <- nrow(data)

    # a ML model train by caret
    if (inherits(final_model, "train")) {

      # save train data, change name and position
      train_data <- final_model[["trainingData"]][1:n,]
      names(train_data)[1] <- "class_efficiency"

      train_data <- train_data[,c(2:length(train_data),1)]

    } else {

      train_data <- final_model[["data"]][1:n,]
      train_data$class_efficiency <- ifelse(train_data$class_efficiency == 1, "efficient", "not_efficient")
      train_data$class_efficiency <- factor(
        train_data$class_efficiency,
        levels = levels_order
      )

    }

  }

  # target data set; by default is "train"
  if (target == "train") {

    # a ML model train by caret
    if (inherits(final_model, "train")) {

      # save train data, change name and position
      target_data <- final_model[["trainingData"]]
      names(target_data)[1] <- "class_efficiency"

      target_data <- target_data[,c(2:length(target_data),1)]

    } else {

      target_data <- final_model[["data"]]
      target_data$class_efficiency <- ifelse(target_data$class_efficiency == 1, "efficient", "not_efficient")
      target_data$class_efficiency <- factor(
        target_data$class_efficiency,
        levels = levels_order
      )

    }

  } else if (target == "real") {

    n <- nrow(data)

    # a ML model train by caret
    if (inherits(final_model, "train")) {

      # save train data, change name and position
      target_data <- final_model[["trainingData"]][1:n,]
      names(target_data)[1] <- "class_efficiency"

      target_data <- target_data[,c(2:length(target_data),1)]

    } else {

      target_data <- final_model[["data"]][1:n,]
      target_data$class_efficiency <- ifelse(target_data$class_efficiency == 1, "efficient", "not_efficient")
      target_data$class_efficiency <- factor(
        target_data$class_efficiency,
        levels = levels_order
      )

    }

  }

  output <- list(
    train_data = train_data,
    target_data = target_data
  )

  return(output)

}
