#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param method Parameters for controlling the training process (from the \code{'caret'} package).
#' @param parameters A \code{list} of selected machine learning models and their hyperparameters.
#' @param trControl A \code{list} of selected machine learning learning.
#' @param metric_priority dfdfd
#'
#' @importFrom caret train createFolds trainControl
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#'
#' @return It returns a \code{list} with the chosen model.

train_PEAXAI <- function (
    data, method, parameters, trControl, metric_priority
    ) {

  # general function to get performance
  PEAXAIsummaryFunction <- function (data, lev = NULL, model = NULL) {

    if(any(is.na(data$pred))) {

      out_names <- c("Accuracy", "Kappa",
                     "Recall", "Specificity", "Precision", "F1", "Balanced Accuracy",
                     "ROC-AUC", "PR-AUC")

      performance <- setNames(rep(NA, length(out_names)), out_names)

      return(performance)

      }

    # levels
    levls <- c("efficient", "not_efficient")

    # predictions
    y_hat <- data$pred
    y_hat <- factor(y_hat, levels = levls)
    y_hat_prob <- data$efficient

    # reference
    y_obs <- data$obs
    y_obs <- factor(y_obs, levels = levls)

    # confusion matrix
    cm <- confusionMatrix(
      data = y_hat,
      reference = y_obs,
      mode = "everything",
      positive = "efficient"
    )

    # caret problem when all units are in the same class
    # not NAs
    if (is.na(cm$byClass[["F1"]])) {
      cm$byClass[["F1"]] <- 0
    }
    if (is.na(cm$byClass[["Precision"]])) {
      cm$byClass[["Precision"]] <- 0
    }

    # ROC-AUC
    roc_obj <- roc(
      response = y_obs,
      predictor = y_hat_prob,
      levels = rev(levls),
      direction = "<",
      quiet = TRUE)

    # PR-AUC
    pr_obj <- pr.curve(
      scores.class0 = y_hat_prob[y_obs == "not_efficient"],
      scores.class1 = y_hat_prob[y_obs == "efficient"],
      curve = TRUE
    )

    performance <- c(
      cm$overall[c("Accuracy", "Kappa")],
      cm$byClass[c("Recall", "Specificity",
                   "Precision", "F1",
                   "Balanced Accuracy")],
      "ROC-AUC" = roc_obj$auc,
      "PR-AUC" = unname(pr_obj$auc.integral)
    )

    return(performance)

  }

  if (trControl[["method"]] == "test_set") {
    trControl <- trainControl(
      method = "none",
      classProbs = TRUE,
      test_hold_out = trControl[["test_hold_out"]],
      summaryFunction = PEAXAIsummaryFunction
    )
  } else {
    trControl <- trainControl(
      method = trControl[["method"]],
      number = trControl[["number"]],
      summaryFunction = PEAXAIsummaryFunction,
      classProbs = TRUE
    )
  }

  # ----------------------------------------------------------------------------
  # Neural Network -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (method == "nnet") {
    train_PEAXAI
    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "nnet",
      preProcess = parameters[["preProcess"]],
      tuneGrid = parameters[["tuneGrid"]],
      trControl = trControl,
      metric = "Accuracy",

      # nnet (no fine-tuning)
      skip = parameters[["skip"]],
      maxit = parameters[["maxit"]],
      MaxNWts = parameters[["MaxNWts"]],
      trace = parameters[["trace"]]
    )

  } else if (method == "rf") {

    # model_fit <- train(
    #   class_efficiency ~ .,
    #   data = data,
    #   method = "rf",
    #   tuneGrid = hyparameter,
    #   trControl = trainControl(method = "none", classProbs = TRUE),
    #   metric = metric,
    #
    #   # rf (no fine-tuning)
    #   ntree = arguments[["ntree"]]
    # )

  } else if (method == "svmPoly") {

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "svmPoly",
      preProcess = parameters[["preProcess"]],
      tuneGrid = parameters[["tuneGrid"]],
      trControl = trControl,
      metric = "Accuracy",
      prob.model = TRUE
    )

  } else if (method == "glm") {


    if (parameters[["weights"]][1] == "dinamic") {
      w0 <- nrow(data) / (2 * length(which(data$class_efficiency == "not_efficient")))
      w1 <- nrow(data) / (2 * length(which(data$class_efficiency == "efficient")))
    } else if (is.data.frame(parameters[["weights"]])) {
      w0 <- parameters[["weights"]][["w0"]]
      w1 <- parameters[["weights"]][["w1"]]
    } else {
      w0 <- 1
      w1 <- 1
    }

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "glm",
      family = binomial(),
      trControl = trControl,
      weights  = ifelse(data$class_efficiency == "efficient", w1, w0),
      metric = metric_priority[1]
    )

    if (trControl[["method"]] != "none") {
      model_fit[["results"]] <- model_fit[["results"]][-1]

      model_fit[["results"]] <- cbind(w0, w1, model_fit[["results"]])
    }

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
#
# train_glm <- function (
#     data, method, arguments
# ) {
#
#   # prediction type
#   type <- "response"
#   levels_order <- c("not_efficient", "efficient")
#   data$class_efficiency <- factor(
#     data$class_efficiency,
#     levels = levels_order)
#
#   if (is.null(arguments[["weights"]])) {
#
#     weights <- NULL
#
#   } else if (arguments[["weights"]][1] == "dinamic") {
#
#     w0 <- nrow(data) / (2 * length(which(data$class_efficiency == "not_efficient")))
#     w1 <- nrow(data) / (2 * length(which(data$class_efficiency == "efficient")))
#
#     weights <-  ifelse(data$class_efficiency == "efficient", w1, w0)
#
#   } else {
#
#     weights <- ifelse(
#       data$class_efficiency == "efficient",
#       arguments[["weights"]][["w1"]], arguments[["weights"]][["w0"]]
#     )
#
#   }
#
#   # fit the glm model
#   model_fit <- glm(
#     class_efficiency ~.,
#     data = data,
#     family = arguments[["family"]],
#     weights = weights
#   )
#
#   model_fit <- step(
#     model_fit,
#     direction = arguments[["direction"]],
#     trace = arguments[["trace"]])
#
#   return(model_fit)
#
# }
