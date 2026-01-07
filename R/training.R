#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param method Parameters for controlling the training process (from the \code{'caret'} package).
#' @param parameters A \code{list} of selected machine learning models and their hyperparameters.
#' @param trControl A \code{list} of selected machine learning learning.
#' @param metric_priority A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_Accuracy"} due to (normally) unbalanced data.
#' @param seed  Integer. Seed for reproducibility.

#'
#' @importFrom caret train trainControl
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#'
#' @return It returns a \code{list} with the chosen model.

train_PEAXAI <- function (
    data, method, parameters, trControl, metric_priority, seed
    ) {

  # general function to get performance
  PEAXAIsummaryFunction <- function (data, lev = NULL, model = NULL) {

    if(any(is.na(data$pred))) {

      out_names <- c("Accuracy", "Kappa",
                     "Recall", "Specificity", "Precision", "F1", "Balanced_Accuracy", "G_mean",
                     "ROC-AUC", "PR-AUC",
                     "Cross_Entropy", "Cross_Entropy_Efficient")

      performance <- setNames(rep(NA, length(out_names)), out_names)

      return(performance)

      }

    # levels
    levls <- c("efficient", "not_efficient")

    # predictions
    y_hat <- data$pred
    y_hat <- factor(y_hat, levels = levls)
    if(levels(y_hat) != c("efficient", "not_efficient")) browser()
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

    # G-mean
    gmean <- sqrt(cm$byClass["Recall"]*cm$byClass["Specificity"])

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

    # cross_entropy
    y_bin <- ifelse(y_obs == "efficient", 1, 0)
    p <- y_hat_prob

    # no log(0)
    eps <- 1e-15
    p_clipped <- pmin(pmax(p, eps), 1 - eps)

    cross_entropy <- -mean(
      y_bin * log(p_clipped) + (1 - y_bin) * log(1 - p_clipped)
    )

    # cross entropy efficient class
    p <- y_hat_prob[y_bin == 1]
    y_bin <- y_bin[y_bin == 1]

    # no log(0)
    eps <- 1e-15
    p_clipped <- pmin(pmax(p, eps), 1 - eps)

    cross_entropy_efficient <- -mean(
      y_bin * log(p_clipped) + (1 - y_bin) * log(1 - p_clipped)
    )

    performance <- c(
      cm$overall[c("Accuracy", "Kappa")],
      cm$byClass[c("Recall", "Specificity",
                   "Precision", "F1")],
      "Balanced_Accuracy" = cm$byClass[["Balanced Accuracy"]],
      "G_mean" = unname(gmean),
      "ROC_AUC" = roc_obj$auc,
      "PR_AUC" = unname(pr_obj$auc.integral),
      "Cross_Entropy" = cross_entropy,
      "Cross_Entropy_Efficient" = cross_entropy_efficient
    )

    return(performance)

  }

  # trControl actualize
  # if (trControl[["method"]] == "cv") {
  #   trControl <- trainControl(
  #     method = trControl[["method"]],
  #     number = trControl[["number"]],
  #     summaryFunction = PEAXAIsummaryFunction,
  #     classProbs = TRUE,
  #     savePredictions = "all"
  #   )
  # } else if (trControl[["method"]] == "repeatedcv") {
  #   trControl <- trainControl(
  #     method = trControl[["method"]],
  #     number = trControl[["number"]],
  #     repeats = trControl[["repeats"]],
  #     summaryFunction = PEAXAIsummaryFunction,
  #     classProbs = TRUE,
  #     savePredictions = "all"
  #   )
  # }
  trControl <- trainControl(method = "none", classProbs = TRUE)

  # ----------------------------------------------------------------------------
  # Neural Network -------------------------------------------------------------
  # ----------------------------------------------------------------------------

  set.seed(seed)
  if (method == "nnet") {

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

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "rf",
      tuneGrid = parameters[["tuneGrid"]],
      trControl = trControl,
      metric = "Accuracy",

      # rf (no fine-tuning)
      ntree = parameters[["ntree"]]
    )

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

  } else if (method == "svmRadial") {

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "svmRadial",
      preProcess = parameters[["preProcess"]],
      tuneGrid = parameters[["tuneGrid"]],
      trControl = trControl,
      metric = "Accuracy",
      prob.model = TRUE
    )

  } else if (method == "glm") {

    if (is.null(parameters[["weights"]])) {

      w0 <- 1
      w1 <- 1

    } else {

      if (parameters[["weights"]][1] == "dinamic") {
        w0 <- nrow(data) / (2 * length(which(data$class_efficiency == "not_efficient")))
        w1 <- nrow(data) / (2 * length(which(data$class_efficiency == "efficient")))
      } else if (is.data.frame(parameters[["weights"]])) {
        w0 <- parameters[["weights"]][["w0"]]
        w1 <- parameters[["weights"]][["w1"]]
      }

    }

    model_fit <- train(
      class_efficiency ~ .,
      data = data,
      method = "glm",
      family = binomial(),
      trControl = trControl,
      weights  = ifelse(data$class_efficiency == "efficient", w1, w0),
      metric = "Accuracy"
    )

    if (trControl[["method"]] != "none") {
      model_fit[["results"]] <- model_fit[["results"]][-1]

      model_fit[["results"]] <- cbind(w0, w1, model_fit[["results"]])
    }

  }

  return(model_fit)

}
