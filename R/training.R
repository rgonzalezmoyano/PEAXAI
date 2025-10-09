#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.

#'
#' @importFrom caret train twoClassSummary
#' @importFrom magrittr arrange
#'
#' @return It returns a \code{list} with the chosen model.

train_ml <- function (
    data, trControl, methods, metric
    ) {

  # list with best configuration
  best_model_fit <- vector("list", length = length(method))

  browser()

  for (a in 1:length(methods)) {

    model <- train(
      class_efficiency ~.,
      data = data,
      method = methods[],
      metric = metric,
      tuneGrid = tunegrid,
      trControl = control)

  }


  for (a in 1:length(methods)) {

      # parameters grid
      tune_grid <- unique(expand.grid(methods[[a]]$hyparams))

      # avoid messages for some methods
      verb_methods <- c("gbm", "svmPoly")

      if (names(methods[a]) == "rf") {

        modellist <- list()

        # ntree parameter change
        for (ntree in methods[[a]]$options$ntree) {

          # Tune model rf
          model <- train (
            form = class_efficiency ~ .,
            data = data,
            method = names(methods[a]),
            trControl = trControl,
            tuneGrid = tune_grid,
            ntree = ntree,
            metric = metric
          )

          key <- toString(ntree)
          modellist[[key]] <- c(method = names(methods[a]), ntree = ntree, model$results)

        }

        # dataframe results
        model_rf <- matrix(
          data = NA,
          ncol = 21,
          nrow = length(methods[[a]]$options$ntree)
        )

        # to dataframe
        model_rf <- as.data.frame(model_rf)

        # paste iteration intormation
        for (row in 1:length(methods[[a]]$options$ntree)) {

          model_rf[row, ] <-  modellist[[row]]

        }

        # add names
        names_result <- c("method", "ntree", unique(names(model$results)))
        names(model_rf) <- names_result

        # select best configuration
        selected_model <- model_rf %>%
          arrange(desc(Accuracy), desc(F), desc(Spec), desc(AUC), desc(Kappa))

        best_model_fit[[a]] <- selected_model[1, ]

      } else if (names(methods[a]) == "nnet") {

        # Tune model nnet
        model_nnet <- train (
          form = class_efficiency ~ .,
          data = data,
          method = names(methods[a]),
          trControl = trControl,
          tuneGrid = tune_grid,
          metric = metric,
          maxit = methods$nnet$options$maxit
        )

        # # Tune model nnet with nnet
        # model_nnet <- nnet(
        #   x = as.matrix(iris_scaled[, 1:4]),  # Variables predictoras
        #   y = as.matrix(iris_scaled$Species),  # Etiquetas en formato binario
        #   size = 3,         # Número de neuronas en la capa oculta
        #   softmax = TRUE,   # Activar softmax
        #   maxit = 200,      # Máximo de iteraciones
        #   trace = FALSE     # No mostrar el progreso
        # )

        selected_model <- round(model_nnet[["results"]], 2) %>%
          mutate(Balance_accuracy = (Sens + Spec) / 2) %>%  # Añadir la nueva métrica
          arrange(desc(Balance_accuracy), desc(F), desc(Precision), desc(AUC), desc(Kappa))

        # selected_model$balance <- round(prop.table(table(data$class_efficiency))[1], 1)

        # add names
        selected_model <- cbind(
          data.frame(
            method = names(methods[a])
          ), selected_model
        )

        best_model_fit[[a]] <- selected_model[1, ]

      } else {

        # svm and others methods
        if (names(methods[a]) %in% verb_methods) {

          # Tune models
          model <- train (
            form = class_efficiency ~ .,
            data = data,
            method = names(methods[a]),
            trControl = trControl,
            tuneGrid = tune_grid,
            verbose = FALSE
          )

        } else {
          # Tune models
          model <- train (
            form = class_efficiency ~ .,
            data = data,
            method = names(methods[a]),
            trControl = trControl,
            tuneGrid = tune_grid
          )
        }

        # compute F1 score
        prec <- model[["results"]]["Precision"]
        sens <- model[["results"]]["Sens"]

        model[["results"]]["F1"] <- (2 * prec * sens) / (sens + prec)

        # select best configuration
        best_config <- model[["results"]] %>%
          arrange(desc(F1), desc(Spec), desc(AUC), desc(Kappa), desc(Accuracy))

        names_result <- c("method", unique(names(model$results)))

        #best_model_fit[[a]] <- best_config[1, ]

        best_model_fit[[a]] <- cbind(model$method, best_config[1, ])
        best_model_fit[[a]] <- cbind(model$method, best_config[1, ])

        names(best_model_fit[[a]]) <- names_result
        names(best_model_fit[a]) <- model$method
      }

  }

  return(best_model_fit)

}
