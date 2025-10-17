#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param method A \code{list} of selected machine learning models and their hyperparameters.
#' @param arguments A \code{list}
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.

#'
#' @importFrom caret train createFolds

#'
#' @return It returns a \code{list} with the chosen model.

train_ml <- function (
    data, trControl, method, arguments, metric
    ) {

  # first, create k-folds
  folds <- createFolds(data$class_efficiency, k = 5) # lista: indica qué filas usar como test en cada iteración

  for (hyparameter_i in 1:nrow(arguments[["tuneGrid"]])) {

    hyparameter <- arguments[["tuneGrid"]][hyparameter_i, ]

    if (method == "glm") {
      type = "response"
    } else {
      type = "prob"
    }

    for (fold in folds) {

      training_fold <- data[-fold, ]
      test_fold <- data[fold, ]

      if (method == "nnet") {

        model_fit <- train(
          class_efficiency ~ .,
          data = training_fold,
          method = "nnet",
          preProcess = arguments[["preProcess"]],
          tuneGrid = hyparameter,
          trControl = trainControl(method = "none", classProbs = TRUE),
          metric = metric,

          # nnet (no fine-tuning)
          skip      = arguments[["skip"]],
          maxit     = arguments[["maxit"]],
          MaxNWts   = arguments[["MaxNWts"]],
          trace     = arguments[["trace"]]
        )

      } else if (method == "rf") {

        model_fit <- train(
          class_efficiency ~ .,
          data = data,
          method = "rf",
          tuneGrid = arguments[["tuneGrid"]],
          trControl = trControl,
          metric = metric,

          # rf (no fine-tuning)
          ntree = arguments[["ntree"]]
        )

      }


      # test performance
      test_fold$pred <- predict(model_fit, newdata = test_fold[, setdiff(names(test_fold), "class_efficiency")], type = type)[,1]
      browser()
      data$obs  <- factor(data$obs,  levels = lev)

      cm <- confusionMatrix(
        data = data$pred,
        reference = data$obs,
        mode = "everything",
        positive = "efficient"
      )

      # ROC-AUC
      roc_obj <- pROC::roc(
        response = data$obs,
        predictor = data$efficient,
        levels = rev(lev), # primero el negativo
        direction = "<",
        quiet = TRUE)

      # PR-AUC
      ppos <- data[["efficient"]]

      pr_obj <- PRROC::pr.curve(
        scores.class0 = data[["efficient"]] [data$obs == "efficient"],
        scores.class1 = data[["efficient"]] [data$obs == "not_efficient"],
        curve = TRUE
      )

      # Entropy/Calibration metrics

      eps <- 1e-15
      y <- as.integer(data$obs == "efficient")

      pcl <- pmin(pmax(data[["efficient"]], eps), 1 - eps)
      LogLoss <- -mean(y * log(pcl) + (1 - y) * log(1 - pcl))
      # PredEntropy_bits <- -mean(pcl * log2(pcl) + (1 - pcl) * log2(1 - pcl))
      # Brier <- mean((pcl - y)^2)

      out <- c(
        cm$overall[c("Accuracy", "Kappa")],
        cm$byClass[c("Recall", "Specificity",
                     "Precision", "F1",
                     "Balanced Accuracy")],
        "ROC" = roc_obj$auc,
        "PR-AUC" = unname(pr_obj$auc.integral),
        "LogLoss" = LogLoss
        # "PredEntropy_bits" = PredEntropy_bits,
        # "Brier" = Brier
      )

    }

  }

  return(model_fit)

}
