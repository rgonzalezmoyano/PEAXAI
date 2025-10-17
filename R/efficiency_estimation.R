#' @title Training Classification Models to Estimate Efficiency
#'
#' @description
#' Trains one or multiple classification algorithms to identify Pareto-efficient
#' decision-making units (DMUs). It jointly searches model hyperparameters and the
#' class-balancing level (e.g., synthetic samples via SMOTE) using k-fold cross-
#' validation or a train/validation/test split, selecting the configuration that
#' maximizes the specified metric(s). Returns, for each technique, the best fitted
#' model together with training summaries, performance metrics, and the selected
#' balancing level.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#' @param RTS Text string or number defining the underlying DEA technology /
#'   returns-to-scale assumption (default: \code{"vrs"}). Accepted values:
#'   \describe{
#'     \item{\code{0} / \code{"fdh"}}{Free disposability hull, no convexity assumption.}
#'     \item{\code{1} / \code{"vrs"}}{Variable returns to scale, convexity and free disposability.}
#'     \item{\code{2} / \code{"drs"}}{Decreasing returns to scale, convexity, down-scaling and free disposability.}
#'     \item{\code{3} / \code{"crs"}}{Constant returns to scale, convexity and free disposability.}
#'     \item{\code{4} / \code{"irs"}}{Increasing returns to scale (up-scaling, not down-scaling), convexity and free disposability.}
#'     \item{\code{5} / \code{"add"}}{Additivity (scaling up and down, but only with integers), and free disposability.}
#'   }
#' @param balance_data Indicate the number of efficient and not efficient units.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param importance_method A \code{string} specifying the method to determine the relative importance of variables. Default includes \code{"SHAP"}.
#' @param hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#' @param test_hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#' @param scenarios Level of efficiency to peer.
#'
#' @importFrom caret train createDataPartition confusionMatrix
#' @importFrom dplyr %>% arrange
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#'
#' @return A \code{"cafee"} object.
#'
#' @export

PEAXAI_fitting <- function (
    data, x, y, RTS, trControl, methods,
    metric, importance_method, hold_out, test_hold_out = 0.2,
    balance_data = 0, scenarios = 0.75
    ) {

  # ----------------------------------------------------------------------------
  # pre-processing -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # Check the data
  data <- preprocessing(
    data = data,
    x = x,
    y = y
  )

  # reorder index 'x' and 'y' in data
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  # number of inputs / outputs as inputs and number of outputs
  nX <- length(x)
  nY <- length(y)

  # add informtation about training
  if(trControl[["method"]] == "none") {
    trControl[["test_hold_out"]] <- test_hold_out
  }

  # save a copy before add class_efficiency
  copy_data_no_label <- data

  # ----------------------------------------------------------------------------
  # Step 1: Data labeling ------------------------------------------------------
  # ----------------------------------------------------------------------------
  data <- label_efficiency(
    data = data,
    x = x,
    y = y,
    RTS = RTS
  )

  info_imb_rate <- round(prop.table(table(data$class_efficiency))[1], 4) * 100

  message(paste0("The dataset has an imbalance rate of ", info_imb_rate, "%."))

  # ----------------------------------------------------------------------------
  # Step 2: Create validation set ----------------------------------------------
  # ----------------------------------------------------------------------------
  # create datasets: (train + test) and (validation)
  if (is.null(hold_out)) {

    # all data is use to train and for validation
    valid_data <- data
    train_data <- data

  } else {

    # validation set
    valid_index <- createDataPartition(
     data$class_efficiency,
      p = hold_out,
      list = FALSE)

    valid_data <- data[valid_index, ]
    train_data <- data[-valid_index,]
    all_data <- data

  }

  # ----------------------------------------------------------------------------
  # Step 3: ML model training --------------------------------------------------
  # ----------------------------------------------------------------------------
  # metrics for model evaluation
  MySummary <- function(data, lev = NULL, model = NULL) {

    data$pred <- factor(data$pred, levels = lev)
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
    pr_obj <- PRROC::pr.curve(
      scores.class0 = data[["efficient"]] [data$obs == "efficient"],
      scores.class1 = data[["efficient"]] [data$obs == "not_efficient"],
      curve = TRUE
    )

    # # Entropy/Calibration metrics
    # eps <- 1e-15
    # y <- as.integer(data$obs == "efficient")
    #
    # pcl <- pmin(pmax(data[["efficient"]], eps), 1 - eps)
    # LogLoss <- -mean(y * log(pcl) + (1 - y) * log(1 - pcl))
    # PredEntropy_bits <- -mean(pcl * log2(pcl) + (1 - pcl) * log2(1 - pcl))
    # Brier <- mean((pcl - y)^2)

    out <- c(
      cm$overall[c("Accuracy", "Kappa")],
      cm$byClass[c("Recall", "Specificity",
                   "Precision", "F1",
                   "Balanced Accuracy")],
      "ROC" = roc_obj$auc,
      "PR-AUC" = unname(pr_obj$auc.integral)
      # "LogLoss" = LogLoss
      # "PredEntropy_bits" = PredEntropy_bits,
      # "Brier" = Brier
    )

    return(out)

  }

  trControl[["summaryFunction"]] <- MySummary

    # --------------------------------------------------------------------------
    # Step 3.1: Addressing imbalance rate --------------------------------------
    # --------------------------------------------------------------------------

  if (is.null(balance_data)) {

    train_data_SMOTE <- train_data
    real_balance <- prop.table(table(train_data$class_efficiency))[["efficient"]]
    datasets_to_train <- list(train_data)
    names(datasets_to_train)[1] <- paste0(as.character(round(real_balance, 4)),"*")

  } else {

    # determine complete facets
    train_data_SMOTE <- SMOTE_data(
      data = train_data,
      x = x,
      y = y,
      RTS = RTS,
      balance_data = balance_data
    )

    real_balance <- prop.table(table(train_data$class_efficiency))[["efficient"]]

    # add no balance scenario
    balance <- c(real_balance, balance_data)

    datasets_to_train <- append(train_data_SMOTE, list(train_data), after = 0)
    names(datasets_to_train)[1] <- paste0(as.character(round(real_balance, 4)),"*")
    names(datasets_to_train)
  }


    # --------------------------------------------------------------------------
    # Step 3.2: Select hyperparameters -----------------------------------------
    # --------------------------------------------------------------------------

  # save model and performance
  best_models <- vector("list", length(methods))
  names(best_models) <- names(methods)

  best_performance  <- vector("list", length(methods))
  names(best_performance) <- names(methods)

  # extra: more information about fitting
  performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
  names(performance_train_all_by_dataset) <- names(datasets_to_train)

  # only the best by imbalance and ML method
  best_performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
  names(best_performance_train_all_by_dataset) <- names(datasets_to_train)

  for (dataset in names(datasets_to_train)) {

    message(paste0("Case: ", dataset, " imbalance rate"))

    if (trControl[["method"]] == "cv") {

      # create k-folds. Same folds for every ML method
      folds <- createFolds(
        datasets_to_train[[dataset]]$class_efficiency,
        k = trControl[["number"]])

    } else if (trControl[["method"]] == "none") {

      # 1 fold trin, 2 fold test. they have different nrows
      folds <- vector("list", length = 2)
      names(folds) <- c("Fold1", "Fold2")

      # determine which samples are test
      test_fold <- createDataPartition(
        datasets_to_train[[dataset]]$class_efficiency,
        p = trControl[["test_hold_out"]])

      # save training and test sets
      folds[["Fold1"]] <- test_fold$Resample1

      # folds[["Fold1"]] <- setdiff(1:nrow(datasets_to_train[[dataset]]), test_fold[[1]])
      folds[["Fold2"]] <- setdiff(1:nrow(datasets_to_train[[dataset]]), test_fold[[1]])

    }

    performance_train_all_by_method <- vector("list", length(methods))
    names(performance_train_all_by_method) <- names(methods)

    best_performance_train_all_by_method <- vector("list", length(methods))
    names(best_performance_train_all_by_method) <- names(methods)

    for (method_i in names(methods)) {

      message(paste0("Training ", method_i, " method."))

      arguments <- methods[[method_i]]

      # if (method_i == "glm") {
      #   arguments[["tuneGrid"]] <- data.frame(1)
      # }

      # save best model if the new if better
      best_record   <- NULL

      # save performance by tuneGrid
      performance_by_tuneGrid <- NULL

      # information_performance <- NULL
      if(method_i == "glm") {
        arguments[["tuneGrid"]] <- data.frame(1)
      }

      for (hyparameter_i in 1:nrow(arguments[["tuneGrid"]])) {

        hyparameter <- arguments[["tuneGrid"]][hyparameter_i, ]
        hyparameter <- as.data.frame(hyparameter)
        names(hyparameter) <- names(arguments[["tuneGrid"]])

        if (method_i == "glm") {

          type <- "response"
          levels <- c(0,1)

        } else {

          type <- "prob"
          levels <- c("efficient", "not_efficient")

        }

        # for each fold, save performance. After, calculate the mean
        performance_by_fold <- NULL
        iter <- 0
        for (fold in folds) {

          iter <- iter + 1
          if (trControl[["method"]] == "none" & iter == 2) {
            next
          }

          training_fold <- datasets_to_train[[dataset]][-fold, ]
          test_fold <- datasets_to_train[[dataset]][fold, ]

          if (method_i == "nnet") {

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

          } else if (method_i == "rf") {

            model_fit <- train(
              class_efficiency ~ .,
              data = training_fold,
              method = "rf",
              tuneGrid = hyparameter,
              trControl = trainControl(method = "none", classProbs = TRUE),
              metric = metric,

              # rf (no fine-tuning)
              ntree = arguments[["ntree"]]
            )

          } else if (method_i == "glm") {

            if (iter == 1) {
              save_weights <- arguments[["weights"]]
            } else {
              arguments[["weights"]] <- save_weights
            }

            if (is.factor(training_fold$class_efficiency)) {
              training_fold$class_efficiency <- ifelse(training_fold$class_efficiency == "efficient", 1, 0)
            }
            # if (iter == 2 & method_i == "glm") {browser()}
            # Is there weights?
            if(!is.null(arguments[["weights"]])) {
              if (arguments[["weights"]] == "dinamic") {

                # original_weights <- arguments[["weights"]]

                w0 <- nrow(training_fold) / (2 * length(which(training_fold$class_efficiency == 0)))
                w1 <- nrow(training_fold) / (2 * length(which(training_fold$class_efficiency == 1)))

                weights <-  ifelse(training_fold$class_efficiency == 1, w1, w0)

                # save weights
                arguments[["weights"]] <- weights

              }
            }

            model_fit <- glm(
              class_efficiency ~.,
              data = training_fold,
              family = arguments[["family"]],
              weights = arguments[["weights"]])

            # summary(model_fit)

            model_fit <- step(model_fit, direction = arguments[["direction"]], trace = arguments[["trace"]])
            # summary(model_fit)

          }

          # ----------------------------------------------------------------------
          # test performance -----------------------------------------------------
          # ----------------------------------------------------------------------
          # change to glm
          if (method_i == "glm") {
            test_fold$class_efficiency <- ifelse(test_fold$class_efficiency == "efficient", 1, 0)
            y_hat <- predict(model_fit, newdata = test_fold[, setdiff(names(test_fold), "class_efficiency")], type = type)


          } else {
            # predicted data
            y_hat <- predict(model_fit, newdata = test_fold[, setdiff(names(test_fold), "class_efficiency")], type = type)[,1]
          }

          y_hat_prob <- y_hat

          if (method_i == "glm") {

            # Logistic regression: 1 not_efficient level, 2 efficient level
            y_hat <- ifelse(y_hat > 0.5, levels[2], levels[1])

            # observed data
            y_obs  <- factor(
              test_fold$class_efficiency,
              levels = levels
            )

          } else {

            # ML general: 1 efficient level, 2 not_efficient level
            y_hat <- ifelse(y_hat > 0.5, levels[1], levels[2])

            # observed data
            y_obs  <- test_fold$class_efficiency

          }

          # change to factor
          y_hat <- factor(
            y_hat,
            levels = levels)

          if (method_i == "glm") {
            cm <- confusionMatrix(
              data = y_hat,
              reference = y_obs,
              mode = "everything",
              positive = "1"
            )
          } else {
            cm <- confusionMatrix(
              data = y_hat,
              reference = y_obs,
              mode = "everything",
              positive = "efficient"
            )
          }


          if (!method_i == "glm") {
            # first the negative level
            levels <- rev(levels)
          }

          # ROC-AUC
          roc_obj <- pROC::roc(
            response = y_obs,
            predictor = y_hat_prob,
            levels = levels,
            direction = "<",
            quiet = TRUE)

          if (!method_i == "glm") {
            # again the origina levels
            levels <- rev(levels)
          }

          if (method_i == "glm") {
            first_eff <- 2
            second_not_eff <- 1

          } else {
            first_eff <- 1
            second_not_eff <- 2
          }

          # PR-AUC
          pr_obj <- PRROC::pr.curve(
            scores.class0 = y_hat_prob[y_obs == levels[second_not_eff]],
            scores.class1 = y_hat_prob[y_obs == levels[first_eff]],
            curve = TRUE
          )

          # Entropy/Calibration metrics
          # eps <- 1e-15
          # y_entropy <- as.integer(y_obs == "efficient")
          # pcl <- pmin(pmax(y_hat_prob, eps), 1 - eps)
          # LogLoss <- -mean(y_entropy * log(pcl) + (1 - y_entropy) * log(1 - pcl))
          # PredEntropy_bits <- -mean(pcl * log2(pcl) + (1 - pcl) * log2(1 - pcl))
          # Brier <- mean((pcl - y_entropy)^2)

          out <- c(
            cm$overall[c("Accuracy", "Kappa")],
            cm$byClass[c("Recall", "Specificity",
                         "Precision", "F1",
                         "Balanced Accuracy")],
            "ROC" = roc_obj$auc,
            "PR-AUC" = unname(pr_obj$auc.integral)
            # "LogLoss" = LogLoss
            # "PredEntropy_bits" = PredEntropy_bits,
            # "Brier" = Brier
          )

          # not NAs, use all folds to colmeans
          if (is.na(out[["F1"]])) {
            out[["F1"]] <- 0
            out[["Precision"]] <- 0
          }

          if (is.na(out[["Precision"]])) {
            out[["Precision"]] <- 0
          }

          # save prformance
          performance_by_fold <- rbind(performance_by_fold, out)

        } # end performance fold

        # mean of fold performance
        performance <- colMeans(performance_by_fold)

        performance <- t(as.data.frame(performance))
        row.names(performance) <- NULL

        df_hyp <- data.frame(
          data = hyparameter
        )

        if (method_i == "glm") {
          performance <- performance

        } else {

          names(df_hyp) <- names(arguments[["tuneGrid"]])

          performance <- cbind(df_hyp, performance)

        }

        if (is.null(best_record)) {

          best_record <- performance

        } else {

          if (performance[[metric]] > best_record[[metric]]) {

          best_record <- performance

          }

        }

        performance_by_tuneGrid <- rbind(performance_by_tuneGrid, performance)

      } # end tuneGrid

      # save the training results
      performance_train_all_by_method[[method_i]] <- performance_by_tuneGrid
      best_record
      best_performance_train_all_by_method[[method_i]] <- best_record
    }

    performance_train_all_by_dataset[[dataset]] <- performance_train_all_by_method
    best_performance_train_all_by_dataset[[dataset]] <- best_performance_train_all_by_method

  }

    # --------------------------------------------------------------------------
    # Step 3.3: Train the best models (without cross-validation) ---------------
    # --------------------------------------------------------------------------

  # get the best in each imbalance rate
  get_model <- function(lst, model = names(methods)) {
    model <- match.arg(model)
    out <- lapply(names(lst), function(k) {
      x <- lst[[k]][[model]]
      if (is.null(x)) return(NULL)              # por si algún dataset no tiene ese modelo
      cbind(model = model, imbalance = k, x, row.names = NULL)
    })
    do.call(rbind, out)
  }

  # train the best models without cross-validation
  save_best_performance <- vector("list", length = length(methods))
  names(save_best_performance) <- names(methods)

  for (method_i in names(methods)) {

    #a dataframe with results
    best_results_train <- get_model(best_performance_train_all_by_dataset, method_i)

    best_results_train <- as.data.frame(best_results_train)

    best_results_train$imbalance <- as.numeric(gsub("\\*", "", best_results_train$imbalance))

    if (method_i == "glm") {

      best_results_train$imbalance <- round(best_results_train$imbalance, 4)

      best_results_train$diff_real_balance[1] <- real_balance

    } else {

      best_results_train$diff_real_balance <- as.numeric(best_results_train$imbalance)

      best_results_train$diff_real_balance[1] <- real_balance

    }

    # distance to real balance
    best_results_train$diff_real_balance <- round(abs(best_results_train$imbalance - real_balance), 4)

    if (method_i == "glm") {

      arguments[["tuneGrid"]] <- data.frame(1)

      best_results_train <- best_results_train %>%
        arrange(desc(.data[[metric]]), diff_real_balance)

    } else {

      names_hyparameters <- names(methods[[method_i]][["tuneGrid"]])

      best_results_train <- best_results_train %>%
        arrange(desc(.data[[metric]]), diff_real_balance)

      best_hyparameters <- best_results_train[1, names_hyparameters]
      best_hyparameters <- as.data.frame(best_hyparameters)
      names(best_hyparameters) <- names_hyparameters

    }

    best_balance <- best_results_train[1, "imbalance"]

    # change to original name
    if (best_balance == round(real_balance, 4)) {
      best_balance <- paste0(best_balance,"*")
    }
    best_balance <- as.character(best_balance)

    arguments <- methods[[method_i]]

    # predict output
    if (method_i == "glm") {

      type <- "response"
      levels <- c(0,1)

    } else {

      type <- "prob"
      levels <- c("efficient", "not_efficient")

    }

    # train the method
    if (method_i == "nnet") {

      model_fit <- train(
        class_efficiency ~ .,
        data = datasets_to_train[[best_balance]],
        method = "nnet",
        preProcess = arguments[["preProcess"]],
        tuneGrid = best_hyparameters,
        trControl = trainControl(method = "none", classProbs = TRUE),
        metric = metric,

        # nnet (no fine-tuning)
        skip = arguments[["skip"]],
        maxit = arguments[["maxit"]],
        MaxNWts = arguments[["MaxNWts"]],
        trace = arguments[["trace"]]
      )

    } else if (method_i == "rf") {

      model_fit <- train(
        class_efficiency ~ .,
        data = datasets_to_train[[best_balance]],
        method = "rf",
        tuneGrid = best_hyparameters,
        trControl = trainControl(method = "none", classProbs = TRUE),
        metric = metric,

        # rf (no fine-tuning)
        ntree = arguments[["ntree"]]
      )

    } else if (method_i == "glm") {

      if (is.factor(datasets_to_train[[best_balance]][["class_efficiency"]])) {
        datasets_to_train[[best_balance]]$class_efficiency <- ifelse(datasets_to_train[[best_balance]]$class_efficiency == "efficient", 1, 0)
      }

      # Is there weights?
      if(!is.null(arguments[["weights"]])) {
        if (arguments[["weights"]] == "dinamic") {

          # original_weights <- arguments[["weights"]]

          w0 <- nrow(datasets_to_train[[best_balance]]) / (2 * length(which(datasets_to_train[[best_balance]]$class_efficiency == 0)))
          w1 <- nrow(datasets_to_train[[best_balance]]) / (2 * length(which(datasets_to_train[[best_balance]]$class_efficiency == 1)))

          weights <- ifelse(datasets_to_train[[best_balance]]$class_efficiency == 1, w1, w0)

          # save weights
          arguments[["weights"]] <- weights

        }
      }

      model_fit <- glm(
        class_efficiency ~.,
        data = datasets_to_train[[best_balance]],
        family = arguments[["family"]],
        weights = arguments[["weights"]])

      model_fit <- step(model_fit, direction = arguments[["direction"]], trace = arguments[["trace"]])

    }

    # ----------------------------------------------------------------------------
    # Validation dataset check ---------------------------------------------------
    # ----------------------------------------------------------------------------

    # change to glm
    if (method_i == "glm") {
      valid_data$class_efficiency <- ifelse(valid_data$class_efficiency == "efficient", 1, 0)
      y_hat <- predict(model_fit, newdata = valid_data[, setdiff(names(valid_data), "class_efficiency")], type = type)

    } else {
      # predicted data
      y_hat <- predict(model_fit, newdata = valid_data[, setdiff(names(valid_data), "class_efficiency")], type = type)[,1]
    }

    y_hat_prob <- y_hat

    # # predicted data
    # y_hat <- predict(model_fit, newdata = valid_data[, setdiff(names(test_fold), "class_efficiency")], type = type)[,1]
    #
    # y_hat_prob <- y_hat

    if (method_i == "glm") {

      # Logistic regression: 1 not_efficient level, 2 efficient level
      y_hat <- ifelse(y_hat > 0.5, levels[2], levels[1])

      # observed data
      y_obs  <- factor(
        valid_data$class_efficiency,
        levels = levels
      )

    } else {

      # ML general: 1 efficient level, 2 not_efficient level
      y_hat <- ifelse(y_hat > 0.5, levels[1], levels[2])

      # observed data
      y_obs  <- valid_data$class_efficiency

    }

    # change to factor
    y_hat <- factor(
      y_hat,
      levels = levels)

    if (method_i == "glm") {
      cm <- confusionMatrix(
        data = y_hat,
        reference = y_obs,
        mode = "everything",
        positive = "1"
      )
    } else {
      cm <- confusionMatrix(
        data = y_hat,
        reference = y_obs,
        mode = "everything",
        positive = "efficient"
      )
    }


    if (!method_i == "glm") {
      # first the negative level
      levels <- rev(levels)
    }

    # ROC-AUC
    roc_obj <- pROC::roc(
      response = y_obs,
      predictor = y_hat_prob,
      levels = levels,
      direction = "<",
      quiet = TRUE)

    if (!method_i == "glm") {
      # again the origina levels
      levels <- rev(levels)
    }

    if (method_i == "glm") {
      first_eff <- 2
      second_not_eff <- 1

    } else {
      first_eff <- 1
      second_not_eff <- 2
    }

    # PR-AUC
    pr_obj <- PRROC::pr.curve(
      scores.class0 = y_hat_prob[y_obs == levels[second_not_eff]],
      scores.class1 = y_hat_prob[y_obs == levels[first_eff]],
      curve = TRUE
    )

    # Entropy/Calibration metrics
    # eps <- 1e-15
    # y_entropy <- as.integer(y_obs == "efficient")
    # pcl <- pmin(pmax(y_hat_prob, eps), 1 - eps)
    # LogLoss <- -mean(y_entropy * log(pcl) + (1 - y_entropy) * log(1 - pcl))
    # PredEntropy_bits <- -mean(pcl * log2(pcl) + (1 - pcl) * log2(1 - pcl))
    # Brier <- mean((pcl - y_entropy)^2)

    out <- c(
      cm$overall[c("Accuracy", "Kappa")],
      cm$byClass[c("Recall", "Specificity",
                   "Precision", "F1",
                   "Balanced Accuracy")],
      "ROC" = roc_obj$auc,
      "PR-AUC" = unname(pr_obj$auc.integral)
      # "LogLoss" = LogLoss
      # "PredEntropy_bits" = PredEntropy_bits,
      # "Brier" = Brier
    )

    # # not NAs, use all folds to colmeans
    # if (is.na(out[["F1"]])) {
    #   out[["F1"]] <- 0
    #   out[["Precision"]] <- 0
    # }
    #
    # if (is.na(out[["Precision"]])) {
    #   out[["Precision"]] <- 0
    # }

    # save performance in validation set
    best_balance <- data.frame(
      "method" = method_i,
      "best_balance" = best_balance
    )

    if (method_i == "glm") {

      if (!is.null(arguments[["weights"]])) {

        best_hyparameters <- data.frame(
          w0 = round(w0, 4),
          w1 = round(w1, 4)
        )

        # change direction columns
        out <- t(out)

        save_best_performance[[method_i]] <- cbind(best_balance, best_hyparameters, out)

      } else {
        out <- t(out)
        save_best_performance[[method_i]] <- cbind(best_balance, out)

      }

    } else {

      best_hyparameters <-  data.frame(
        best_hyparameters
      )
      names(best_hyparameters) <- names(arguments[["tuneGrid"]])

      # change direction columns
      out <- t(out)

      df_per <- cbind(best_balance, best_hyparameters, out)

      if (nrow(df_per) > 1) {
        t(df_per)
      }

      save_best_performance[[method_i]] <- df_per

    }

  }

  # ----------------------------------------------------------------------------
  # Train best models (all data + balance) -------------------------------------
  # ----------------------------------------------------------------------------

  extract_imbalance <- function(df) {
    # busca nombre de columna plausible
    cand <- c("best_balance")
    col <- intersect(cand, names(df))
    if (length(col) == 0) return(NA_real_)
    vals <- unique(df[[col[1]]])
    # limpia a numérico (quita asteriscos u otros símbolos)
    # as.numeric(gsub("[^0-9.]+", "", as.character(vals)))
  }

  imbal_vec <- sapply(save_best_performance, extract_imbalance)
  imbal_vec <- imbal_vec[!is.na(as.numeric(imbal_vec))]
  new_balance_data <- imbal_vec

  if (is.null(balance_data)) {

    all_data_SMOTE <- all_data

  } else {

    if (length(new_balance_data) != 0) {
      # the best dataset is always the original

      all_data_SMOTE <- SMOTE_data(
        data = all_data,
        x = x,
        y = y,
        RTS = RTS,
        balance_data = unique(as.numeric(new_balance_data))
      )

    }

  }

  # if is non balance dataset needed?
  extract_imbalance <- function(df) {
    # busca nombre de columna plausible
    cand <- c("best_balance")
    col <- intersect(cand, names(df))
    if (length(col) == 0) return(NA_real_)
    vals <- unique(df[[col[1]]])
    # limpia a numérico (quita asteriscos u otros símbolos)
    as.numeric(gsub("[^0-9.]+", "", as.character(vals)))
  }

  imbal_vec <- sapply(save_best_performance, extract_imbalance)

  # safe to change after
  old_real_balance <- round(real_balance, 4)

  if (round(real_balance, 4) %in% imbal_vec) {

    if (all(imbal_vec == round(real_balance, 4))) {
      all_data_SMOTE <- list(all_data)
      names(all_data_SMOTE) <- paste0(as.character(round(real_balance, 4)), "*")
    } else {
      # add no balance dataset
      all_data_SMOTE <- append(all_data_SMOTE, list(all_data), after = 0)
      real_balance <- prop.table(table(all_data$class_efficiency))[1]
      real_balance <- round(real_balance, 4)
      names(all_data_SMOTE)[1] <- paste0(as.character(round(real_balance, 4),"*"))

      names(all_data_SMOTE)
    }

  }

  save_best_models <- vector("list", length = length(names(methods)))
  names(save_best_models) <- names(methods)

  for (method_i in names(methods)) {

    # select the balance dataset in
    best_imbalance_i <- save_best_performance[[method_i]][["best_balance"]]
if(length(best_imbalance_i) > 1) browser()
    if (best_imbalance_i == paste0(as.character(old_real_balance),"*")) {

      best_imbalance_i <- paste0(as.character(round(real_balance, 4)),"*")
      save_best_performance[[method_i]][["best_balance"]] <- best_imbalance_i
    }

    if (method_i == "glm") {

      arguments[["tuneGrid"]] <- data.frame(1)

      # select hyparameters
      names_hyparameters <- c("w0", "w1")

    } else {

      # select hyparameters
      names_hyparameters <- names(methods[[method_i]][["tuneGrid"]])

      # best_results_train <- best_results_train %>%
      #   arrange(desc(.data[[metric]]), diff_real_balance)
      #
      # best_hyparameters <- best_results_train[1, names_hyparameters]
      # best_hyparameters <- as.data.frame(best_hyparameters)
      # names(best_hyparameters) <- names_hyparameters

      #
    }

    # select hyparameters
    # names_hyparameters <- names(methods[[method_i]][["tuneGrid"]])

    arguments <- methods[[method_i]]


    if (method_i == "glm" & is.null(arguments[["weights"]])) {

      best_hyparameters <- NULL

    } else {

      best_hyparameters <- save_best_performance[[method_i]][, names_hyparameters]

      best_hyparameters <- as.data.frame(
        best_hyparameters
      )
      names(best_hyparameters) <- names_hyparameters
    }

    # train the method
    if (method_i == "nnet") {

      model_fit <- train(
        class_efficiency ~ .,
        data = all_data_SMOTE[[best_imbalance_i]],
        method = "nnet",
        preProcess = arguments[["preProcess"]],
        tuneGrid = best_hyparameters,
        trControl = trainControl(method = "none", classProbs = TRUE),
        metric = metric,

        # nnet (no fine-tuning)
        skip = arguments[["skip"]],
        maxit = arguments[["maxit"]],
        MaxNWts = arguments[["MaxNWts"]],
        trace = arguments[["trace"]]
      )

    } else if (method_i == "rf") {

      model_fit <- train(
        class_efficiency ~ .,
        data = all_data_SMOTE[[best_imbalance_i]],
        method = "rf",
        tuneGrid = best_hyparameters,
        trControl = trainControl(method = "none", classProbs = TRUE),
        metric = metric,

        # rf (no fine-tuning)
        ntree = arguments[["ntree"]]
      )

    } else if (method_i == "glm") {

      if (is.factor(all_data_SMOTE[[best_imbalance_i]][["class_efficiency"]])) {
        all_data_SMOTE[[best_imbalance_i]]$class_efficiency <- ifelse(all_data_SMOTE[[best_imbalance_i]]$class_efficiency == "efficient", 1, 0)
      }

      # Is there weights?
      if(!is.null(arguments[["weights"]])) {
        if (arguments[["weights"]] == "dinamic") {

          weights <- save_best_performance[[method_i]][, c("w0", "w1")]

          weights <- ifelse(all_data_SMOTE[[best_imbalance_i]]$class_efficiency == 1, weights[["w1"]], weights[["w0"]])

          # save weights
          arguments[["weights"]] <- weights


        }
      } else {

        weights <- NULL
        # save weights
        arguments[["weights"]] <- weights

      }

      model_fit <- glm(
        class_efficiency ~.,
        data = all_data_SMOTE[[best_imbalance_i]],
        family = arguments[["family"]],
        weights = arguments[["weights"]])

      model_fit <- step(model_fit, direction = arguments[["direction"]], trace = arguments[["trace"]])

    }

    model_fit$results <- save_best_performance[[method_i]]
    save_best_models[[method_i]] <- model_fit

  }

  # message with the best
  df_performance <- NULL

  for (a in names(methods)) {

    idx <- 1:length(names(methods))
    idx_i <- idx[which(names(methods) == a)]

    # models <- save_best_performance[[a]]

    # save parameters
    names_out <- as.data.frame(out)
    names_out <- names(names_out)

    performance <- save_best_performance[[a]][, names_out]

    name_method <- as.data.frame(a)
    names(name_method) <- "method"
    names_out <- cbind(name_method, performance)

    if (is.null(df_performance)) {

      df_performance <- names_out

    } else {

      df_performance <- rbind(df_performance, names_out)

    }

  }

  df_performance <- df_performance %>%
    dplyr::arrange(dplyr::desc(.data[[metric]]), is.na(.data[[metric]]))


  message(paste0("The ML method which has the best performance on ", metric, " was ", df_performance[1,1]))

  output_PEAXAI <- list(
    dataset_labeled = all_data,
    best_models = save_best_models,
    best_performance = save_best_performance,
    train_performace_informtation = performance_train_all_by_dataset
  )

  return(output_PEAXAI)

  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------

  # save train data, change name and position
  train_data <- final_model[["trainingData"]]
  names(train_data)[1] <- "class_efficiency"

  train_data <- train_data[,c(2:length(train_data),1)]

  if (importance_method == "SA") {

    # importance with our model of Caret
    mypred <- function(M, data) {
      return (predict(M, data[-length(data)], type = "prob"))
    }

    # Define methods and measures
    methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")

    levels <- 7

  } else if (importance_method == "SHAP") {

    # the fisrt level is efficient
    train_data$class_efficiency <- factor(
      train_data$class_efficiency,
      levels = c("efficient","not_efficient")
      )

    # matrix of data without label
    X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]

    # predict efficiency
    f_pred <- function(object, newdata) {
      predict(object, newdata = newdata, type = "prob")[, "efficient"]
    }

    #
    shap_model <- fastshap::explain(
      object       = final_model,
      X            = X,
      pred_wrapper = f_pred,
      newdata      = X,
      nsim         = 500 # 2048
    )

    # global importance = mean |SHAP| per variable
    imp <- data.frame(
      feature = colnames(shap_model),
      importance = colMeans(abs(shap_model), na.rm = TRUE)
    )

    # Normalize
    imp_norm <- imp[order(-imp$importance), , drop = FALSE]
    imp_norm$importance_norm <- imp_norm$importance / sum(imp_norm$importance, na.rm = TRUE)
    importance <- imp_norm$importance_norm
    importance

  } else if (importance_method == "PI") {

    # the fisrt level is efficient
    train_data$class_efficiency <- factor(
      train_data$class_efficiency,
      levels = c("efficient","not_efficient")
    )

    # matrix of data without label
    X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]

    # 3) Wrapper de predicción: devuelve P(clase positiva)
    #    (robusto al nombre de columna por si cambiaste niveles después de entrenar)
    f_pred <- function(object, newdata) {

      pr <- as.data.frame(predict(object, newdata = newdata, type = "prob"))

      pos_col <- if ("efficient" %in% names(pr)) {
        "efficient"
      } else {
          names(pr)[1]
      }


      as.numeric(pr[[pos_col]])

    }

    # 4) Construir el Predictor de iml
    pred_obj <- iml::Predictor$new(
      model = final_model,
      data = X,
      y = train_data$class_efficiency,
      predict.function = f_pred,
      type = "prob"
    )

    # Loss Function by AUC
    loss_auc <- function(truth, estimate) {

      # truth puede venir como factor → lo mapeamos a 0/1 con positivo = 1
      y_bin <- if (is.factor(truth)) as.numeric(truth == levels(truth)[1]) else as.numeric(truth)

      if (length(unique(y_bin)) < 2) return(NA_real_)  # guardia por si algún bloque queda con 1 sola clase

      1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))

    }

    # 6) Permutation Importance (repite permutaciones para estabilidad)
    set.seed(0)
    fi <- FeatureImp$new(
      predictor = pred_obj,
      loss = loss_auc,
      compare = "difference",      # caída de rendimiento vs. modelo completo
      n.repetitions = 30           # sube si quieres más estabilidad
    )

    # 7) Tabla de importancias y normalización (mismo formato que usabas con SHAP)
    imp <- fi$results[, c("feature", "importance")]
    imp <- imp[order(-imp$importance), , drop = FALSE]
    imp$importance_norm <- imp$importance / sum(imp$importance, na.rm = TRUE)

    importance <- imp$importance_norm
    importance
  }

  result_importance <- importance
  names(result_importance) <- setdiff(names(train_data), "class_efficiency")

  print(paste("Inputs importance: ", round(sum(result_importance[1:length(x)]))), 5)
  print(paste("Outputs importance: ", round(sum(result_importance[(length(x)+1):(length(x)+length(y))]))), 5)

  # ----------------------------------------------------------------------------
  # get ranking ----------------------------------------------------------------
  # ----------------------------------------------------------------------------

  data_rank <- data[,setdiff(names(train_data), "class_efficiency")]
  data_rank <- as.data.frame(data_rank)

  eff_vector <- apply(data_rank, 1, function(row) {

    row_df <- as.data.frame(t(row))

    colnames(row_df) <- names(data_rank)

    pred <- unlist(predict(final_model, row_df, type = "prob")[1])

    return(pred)
  })

  eff_vector <- as.data.frame(eff_vector)

  id <- as.data.frame(c(1:nrow(data)))
  names(id) <- "id"
  eff_vector <- cbind(id, eff_vector)

  ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]

  # ----------------------------------------------------------------------------
  # to get probabilities scenarios ---------------------------------------------
  # ----------------------------------------------------------------------------
  data_scenario_list <- list()
  metrics_list <- list()
  peer_list <- list()
  peer_weight_list <- list()
  na_count_list <- list()
  n_not_prob_list <- list()

  for (e in 1:length(scenarios)) {
    print(paste("scenario: ", scenarios[e]))
    print(final_model)

    data_scenario <- compute_target(
      data = data[,setdiff(names(train_data), "class_efficiency")],
      x = x,
      y = y,
      final_model = final_model,
      cut_off = scenarios[e],
      imp_vector = result_importance
    )

    if(all(is.na(data_scenario$data_scenario))) {
      print("all na")
      browser()

      # peer
      peer_restult <- NA

      # save_peer
      peer_list[[e]] <- peer_restult

      # main_metrics
      main_metrics <- NA

      # save main_metrics
      metrics_list[[e]] <- main_metrics

      print("pause")

    } else {

      if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {

        data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA

        na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
        data_scenario$betas[na_idx,] <- NA
      }

      data_scenario_list[[e]] <- data_scenario

      # ------------------------------------------------------------------------
      # determinate peer -------------------------------------------------------
      # ------------------------------------------------------------------------

      # first, determinate efficient units
      idx_eff <- which(eff_vector$eff_vector > scenarios[e])

      if (!length(idx_eff) == 0) {

        # save distances structure
        save_dist <- matrix(
          data = NA,
          ncol = length(idx_eff),
          nrow = nrow(data)
        )

        # save weighted distances structure
        save_dist_weight <- matrix(
          data = NA,
          ncol = length(idx_eff),
          nrow = nrow(data)
        )

        # calculate distances
        for (unit_eff in idx_eff) {

          # set reference
          reference <- data[unit_eff, c(x,y)]

          distance <- unname(apply(data[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))

          # get position in save results
          idx_dis <- which(idx_eff == unit_eff)

          save_dist[,idx_dis] <- as.matrix(distance)
        }

        near_idx_eff <- apply(save_dist, 1, function(row) {

          which.min(abs(row))

        })

        peer_restult <- matrix(
          data = NA,
          ncol = 1,
          nrow = nrow(data)
        )

        peer_restult[, 1] <- idx_eff[near_idx_eff]

        # save_peer
        peer_list[[e]] <- peer_restult

        # eval_data[1, c(x, y)]
        # eval_data[3, c(x, y)]
        #
        # eval_data[1, c(x, y)] - eval_data[3, c(x, y)]
        #
        # ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
        #
        # result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
        #
        # sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2))
        #
        # sqrt( sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2)))

        # calculate weighted distances
        result_importance_matrix <- as.data.frame(matrix(
          data = rep(unlist(result_importance[c(x,y)]), each = nrow(data)),
          nrow = nrow(data),
          ncol = ncol(data[,c(x,y)]),
          byrow = FALSE
        ))
        names(result_importance_matrix) <- names(data)[c(x,y)]

        w_eval_data <- data[, c(x, y)] * result_importance_matrix

        for (unit_eff in idx_eff) {

          # set reference
          reference <- data[unit_eff, c(x,y)]

          distance <- unname(apply(data[, c(x, y)], 1, function(row) {
             sqrt((sum(result_importance[c(x,y)] * ((row - reference)^2))))
          }))

          # get position in save results
          idx_dis <- which(idx_eff == unit_eff)
          save_dist_weight[,idx_dis] <- as.matrix(distance)
        }

        near_idx_eff_weight <- apply(save_dist_weight, 1, function(row) {

          which.min(abs(row))

        })

        peer_restult_weight <- matrix(
          data = NA,
          ncol = 1,
          nrow = nrow(save_dist_weight)
        )

        peer_restult_weight[, 1] <- idx_eff[near_idx_eff]

        # save_peer
        peer_weight_list[[e]] <- peer_restult_weight

        # # join data plus betas to metrics for scenario
        # data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
        #
        # # number not scenario
        # n_not_prob <- which(data_metrics$probability < scenarios[e])
        # n_not_prob_list[[e]] <- n_not_prob
        #
        # # count na
        # na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
        # count_na <- length(na_row)
        # na_count_list[[e]] <- count_na
        #
        # # metrics: mean, median, sd
        # main_metrics <- as.data.frame(matrix(
        #   data = NA,
        #   ncol = ncol(data_metrics),
        #   nrow = 3
        # ))
        #
        # # metrics
        # main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
        # main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
        # main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)
        #
        # names(main_metrics) <- names(data_metrics)
        # row.names(main_metrics) <- c("mean", "median", "sd")
        #
        # metrics_list[[e]] <- main_metrics

      } else {

        peer_list[[e]] <- NULL
        na_count_list[[e]] <- nrow(data)
        metrics_list[[e]] <- NULL
      }

      # join data plus betas to metrics for scenario
      data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))

      # number not scenario
      n_not_prob <- which(data_metrics$probability < scenarios[e])
      n_not_prob_list[[e]] <- n_not_prob

      # count na
      na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
      count_na <- length(na_row)
      na_count_list[[e]] <- count_na

      # metrics: mean, median, sd
      main_metrics <- as.data.frame(matrix(
        data = NA,
        ncol = ncol(data_metrics),
        nrow = 3
      ))

      # metrics
      main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
      main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
      main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)

      names(main_metrics) <- names(data_metrics)
      row.names(main_metrics) <- c("mean", "median", "sd")

      metrics_list[[e]] <- main_metrics

    }

  } # end loop scenarios

  final_model <- list(
    final_method = final_method,
    final_imbalance_rate = final_imbalance_rate,
    performance_train_all = performance_train_all,
    save_performance = save_performance,
    final_model = final_model,

    # # train_decision_balance = train_decision_balance,
    # real_decision_balance = selected_real_balance,


    # performance_train_dataset = selected_model,
    # performance_real_data = performance_real_data,
    # importance = importance,
    result_importance = result_importance,
    eff_vector = eff_vector,
    ranking_order = ranking_order,
    peer_list = peer_list,
    peer_weight_list = peer_weight_list,
    data_scenario_list = data_scenario_list,
    metrics_list = metrics_list,
    count_na = na_count_list,
    n_not_prob_list = n_not_prob_list
  )

  return(final_model)

}
