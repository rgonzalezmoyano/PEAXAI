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

train_ml <- function (
    data, method, arguments, hyparameter, metric
    ) {

  # ----------------------------------------------------------------------------
  # Neural Network -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (method == "nnet") {

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "nnet",
      preProcess = arguments[["preProcess"]],
      tuneGrid = hyparameter,
      trControl = trainControl(method = "none", classProbs = TRUE),
      metric = metric,

      # nnet (no fine-tuning)
      skip = arguments[["skip"]],
      maxit = arguments[["maxit"]],
      MaxNWts = arguments[["MaxNWts"]],
      trace = arguments[["trace"]]
    )

  } else if (method == "rf") {

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "rf",
      tuneGrid = hyparameter,
      trControl = trainControl(method = "none", classProbs = TRUE),
      metric = metric,

      # rf (no fine-tuning)
      ntree = arguments[["ntree"]]
    )

  }

  return(model_fit)

}

#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param method Parameters for controlling the training process (from the \code{'caret'} package).
#' @param arguments A \code{list} of selected machine learning models and their hyperparameters.
#'
#' @importFrom caret train createFolds

#'
#' @return It returns a \code{list} with the chosen model.

train_glm <- function (
    data, method, arguments
) {

  # prediction type
  type <- "response"
  levels_order <- c("not_efficient", "efficient")
  data$class_efficiency <- factor(
    data$class_efficiency,
    levels = levels_order)

  if (is.null(arguments[["weights"]])) {

    weights <- NULL

  } else if (arguments[["weights"]][1] == "dinamic") {

    w0 <- nrow(data) / (2 * length(which(data$class_efficiency == "not_efficient")))
    w1 <- nrow(data) / (2 * length(which(data$class_efficiency == "efficient")))

    weights <-  ifelse(data$class_efficiency == "efficient", w1, w0)

  } else {

    weights <- ifelse(
      data$class_efficiency == "efficient",
      arguments[["weights"]][["w1"]], arguments[["weights"]][["w0"]]
    )

  }

  # fit the glm model
  model_fit <- glm(
    class_efficiency ~.,
    data = data,
    family = arguments[["family"]],
    weights = weights
  )

  model_fit <- step(
    model_fit,
    direction = arguments[["direction"]],
    trace = arguments[["trace"]])

  return(model_fit)

}
