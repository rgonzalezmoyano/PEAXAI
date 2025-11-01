#' @title Training Classification Models to Estimate Efficiency
#'
#' @description
#' Trains one or multiple classification algorithms to identify Pareto-efficient
#' decision-making units (DMUs). It jointly searches model hyperparameters and the
#' class-balancing level (synthetic samples via SMOTE) using k-fold cross-
#' validation or a train/validation/test split, selecting the configuration that
#' maximizes the specified metric(s). Returns, for each technique, the best fitted
#' model together with training summaries, performance metrics, and the selected
#' balancing level.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Integer vector with column indices of input variables in \code{data}.
#' @param y Integer vector with column indices of output variables in \code{data}.
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
#' @param trControl A \code{caret::trainControl}-like list that specifies the resampling
#'   strategy; recognized values for \code{$method} include \code{"cv"}, \code{"test_set"},
#'   and \code{"none"}. See \pkg{caret} documentation.
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric_priority A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param hold_out Numeric proportion in (0,1) for validation split (default \code{NULL}).
#'   If \code{NULL}, training and validation use the same data.
#' @param imbalance_rate Optional target(s) for class balance via SMOTE. If \code{NULL},
#'   no synthetic balancing is performed.
#' @param verbose Logical; if \code{TRUE}, prints progress messages (default \code{FALSE}).
#'
#' @importFrom caret train createDataPartition confusionMatrix
#' @importFrom dplyr %>% arrange across
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#'
#' @return A \code{"PEAXAI"} (list) with the best technique, best fitted models and their performance and the results by fold.
#'
#' @export

PEAXAI_fitting <- function (
    data, x, y, RTS = "vrs", imbalance_rate = NULL,
    trControl, methods, metric_priority = "Balanced Accuracy",
    hold_out = NULL, verbose = FALSE
    ) {

  # ----------------------------------------------------------------------------
  # pre-processing -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # check if parameters are well introduced
  validate_parametes(
    data = data,
    x = x,
    y = y,
    RTS = RTS,
    trControl = trControl,
    methods = methods,
    metric_priority = metric_priority,
    imbalance_rate = imbalance_rate,
    hold_out = hold_out,
    verbose = verbose
  )

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  # Check data
  data <- preprocessing(
    data = data,
    x = x,
    y = y
  )

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

  if (isTRUE(verbose)) {
    info_imb_rate <- round(prop.table(table(data$class_efficiency))[1], 4) * 100
    message(
      paste0("The dataset has an imbalance rate of ", info_imb_rate, "%.")
    )
  }

  # ----------------------------------------------------------------------------
  # Step 2: Create validation set ----------------------------------------------
  # ----------------------------------------------------------------------------
  # create datasets: (train + test) and (validation)
  if (is.null(hold_out)) {

    # all data is use to train and for validation
    valid_data <- data
    train_data <- data
    all_data <- data

  } else {

    # validation set
    valid_index <- createDataPartition(
     data$class_efficiency,
      p = hold_out,
      list = FALSE)

    # split data
    valid_data <- data[valid_index, c(x,y)]
    train_data <- data[-valid_index, c(x,y)]

    # label DMUs by technology observed in train
    valid_data <- label_efficiency(
      data = valid_data,
      REF = train_data,
      x = x,
      y = y,
      RTS = RTS
    )

    train_data <- label_efficiency(
      data = train_data,
      REF = train_data,
      x = x,
      y = y,
      RTS = RTS
    )

    all_data <- data

    save_performance_validation <- vector("list", length = length(methods))
    names(save_performance_validation) <- names(methods)

  }

  # ----------------------------------------------------------------------------
  # Step 3: ML model training --------------------------------------------------
  # ----------------------------------------------------------------------------
  # the original train dataset
  real_balance <- prop.table(table(train_data$class_efficiency))[["efficient"]]
  datasets_to_train <- list(train_data)
  names(datasets_to_train)[1] <- paste0(as.character(round(real_balance, 4)),"*")

  # ----------------------------------------------------------------------------
  # Step 3.1: Addressing imbalance rate (if there is convexity) ----------------
  # ----------------------------------------------------------------------------
  RTS_available <- c("1", "vrs")

  if (!is.null(imbalance_rate)) {

    if (as.character(RTS) %in% RTS_available) {

      # determine complete facets
      train_data_SMOTE <- SMOTE_data(
        data = train_data,
        x = x,
        y = y,
        RTS = RTS,
        balance_data = imbalance_rate
      )

      datasets_to_train <- append(train_data_SMOTE, datasets_to_train, after = 0)

    } else {
      message(
        "Without the convexity assumption, SMOTE units cannot be created on the efficient frontier."
      )
    }

  }

  # ----------------------------------------------------------------------------
  # Step 3.2: Performance with different hyperparameters -----------------------
  # ----------------------------------------------------------------------------
  # Choose best hyperparameters via: "cv" or "test_set"
  # If multiple imbalance levels tie, break the tie using their training-set performance.
#comentar
  # save model
  best_models <- vector("list", length(methods))
  names(best_models) <- names(methods)

  # save performance
  best_performance  <- vector("list", length(methods))
  names(best_performance) <- names(methods)

  # more information about fitting (grid)
  performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
  names(performance_train_all_by_dataset) <- names(datasets_to_train)

  # only the best by imbalance and ML method
  best_performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
  names(best_performance_train_all_by_dataset) <- names(datasets_to_train)

  # metrics evaluation
  metric <- metric_priority[1]

  # save results each dataset
  for (dataset in names(datasets_to_train)) {
    if (isTRUE(verbose)) {
      message(paste0("Case: ", dataset, " imbalance rate"))
    }

    # create k-folds. Same folds for every ML method
    folds <- createFolds(
      datasets_to_train[[dataset]]$class_efficiency,
      k = trControl[["number"]]
    )

    # method
    performance_train_all_by_method <- vector("list", length(methods))
    names(performance_train_all_by_method) <- names(methods)

    best_performance_train_all_by_method <- vector("list", length(methods))
    names(best_performance_train_all_by_method) <- names(methods)

    for (method_i in names(methods)) {
      if (isTRUE(verbose)) {
        message(paste0("Training ", method_i, " method."))
      }

      # train model
      model_fit <- train_PEAXAI(
        data = datasets_to_train[[dataset]],
        method = method_i,
        parameters = methods[[method_i]],
        trControl = trControl,
        metric_priority = metric_priority
      )

      performance_train_all_by_method[[method_i]] <- model_fit[["results"]]

      # order to find the best hyperparameter combination
      idx_order <- do.call(
        order,
        c(
          as.list(model_fit[["results"]][, metric_priority, drop = FALSE]),
          list(decreasing = TRUE, na.last = TRUE)
        )
      )

      results_order <- model_fit[["results"]][idx_order, ]

      best_performance_train_all_by_method[[method_i]] <- results_order[1,]

    }

    performance_train_all_by_dataset[[dataset]] <- performance_train_all_by_method
    best_performance_train_all_by_dataset[[dataset]] <- best_performance_train_all_by_method

  } # end addressing imbalance rate

  # ----------------------------------------------------------------------------
  # Step 3.3: Train the best models and their performance ----------------------
  # ----------------------------------------------------------------------------
  # get the best in each imbalance rate
  get_model <- function(lst, model = names(methods)) {
    model <- match.arg(model)
    out <- lapply(names(lst), function(k) {
      x <- lst[[k]][[model]]
      if (is.null(x)) return(NULL)
      cbind(model = model, imbalance = k, x, row.names = NULL)
    })
    do.call(rbind, out)
  }

  # train the best models without cross-validation
  save_performance_train <- vector("list", length = length(methods))
  names(save_performance_train) <- names(methods)

  save_best_model_fit <- vector("list", length = length(methods))
  names(save_best_model_fit) <- names(methods)

  for (method_i in names(methods)) {

    # first, select the best imbalance rate
    # dataframe with results of CV of method_i
    best_results_train <- get_model(best_performance_train_all_by_dataset, method_i)
    best_results_train <- best_results_train[,2:ncol(best_results_train)]

    best_results_train <- as.data.frame(best_results_train)

    best_results_train$imbalance <- as.numeric(gsub("\\*", "", best_results_train$imbalance))
    best_results_train$diff_imbalance <- best_results_train$imbalance - round(real_balance, 4)

    # how similar is the dataset to original dataset
    best_results_train$diff_imbalance <- 1 - best_results_train$diff_imbalance

    # add * in the orginal dataset name
    best_results_train$imbalance[which.max(best_results_train$diff_imbalance)] <- paste0(best_results_train$imbalance[which.max(best_results_train$diff_imbalance)], "*")

    # add difference imbalance
    metric_priority <- c(metric_priority, "diff_imbalance")

    best_results_train <- best_results_train[do.call(order, c(best_results_train[metric_priority], list(decreasing = TRUE))), ]

    best_balance_i <- best_results_train[1, "imbalance"]

    # copy of methods
    best_methods <- methods

    # only the best hyperparameters
    names_tuneGrid <- names(best_methods[[method_i]][["tuneGrid"]])

    if (method_i != "glm") {
      best_methods[[method_i]][["tuneGrid"]] <- best_results_train[1, names_tuneGrid]
    }

    metric_priority <- metric_priority[-length(metric_priority)]

    # train the best model
    # NO VALIDATION SET
    # All data used, performance is obteined of cv results
    model_fit <- train_PEAXAI(
      data = datasets_to_train[[as.character(best_balance_i)]],
      method = method_i,
      parameters = best_methods[[method_i]],
      trControl = trainControl(method = "none", classProbs = TRUE),
      metric_priority = metric_priority
    )

    performance <- best_results_train[1,]

    # save results
    save_performance_train[[method_i]] <- performance
    save_best_model_fit[[method_i]] <- model_fit


    # WITH VALIDATION SET
    # It is necessary to check the performance of the best model without cv on validation set
    # after that, it is necessary to train again WITH ALL data and the best hyperparameters
    if (!is.null(hold_out)) {

      # check performance on valid data
      y_hat <- predict(model_fit, newdata = valid_data, type = "prob")[["efficient"]]
      y_obs <- valid_data$class_efficiency

      if (any(is.na(y_hat))) {

        out_names <- c("Accuracy", "Kappa",
                       "Recall", "Specificity", "Precision", "F1", "Balanced Accuracy",
                       "ROC-AUC", "PR-AUC")

        performance <- setNames(rep(NA, length(out_names)), out_names)

      } else {

        # levels
        levls <- c("efficient", "not_efficient")

        # predictions
        y_hat_prob <- y_hat
        y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
        y_hat <- factor(y_hat, levels = levls)

        # reference
        y_obs <- factor(y_obs, levels = levls)

        # confusion matrix
        cm <- confusionMatrix(
          data = y_hat,
          reference = y_obs,
          mode = "everything",
          positive = "efficient"
        )

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

        # save a copy
        same_performance <- performance

        best_imbalance_rate <- as.data.frame(best_balance_i)
        names(best_imbalance_rate) <- "imbalance_rate"

        best_hyperparameter <- best_results_train[1, names_tuneGrid]

        if (method_i == "glm") {

          parameters <- methods[["glm"]]

          if (parameters[["weights"]][1] == "dinamic") {
            w0 <- nrow(datasets_to_train[[as.character(best_balance_i)]]) / (2 * length(which(datasets_to_train[[as.character(best_balance_i)]]$class_efficiency == "not_efficient")))
            w1 <- nrow(datasets_to_train[[as.character(best_balance_i)]]) / (2 * length(which(datasets_to_train[[as.character(best_balance_i)]]$class_efficiency == "efficient")))
          } else if (is.data.frame(parameters[["weights"]])) {
            w0 <- parameters[["weights"]][["w0"]]
            w1 <- parameters[["weights"]][["w1"]]
          } else {
            w0 <- 1
            w1 <- 1
          }

          best_hyperparameter <- data.frame(w0, w1)

        }

        performance <- t(performance)

        performance <- cbind(
          best_imbalance_rate,
          best_hyperparameter,
          performance)

      }

      save_performance_validation[[method_i]] <- performance

    } # end hold out

  } # end method

  # show results without hold out
  if(is.null(hold_out)) {

    if (is.null(hold_out)) {

      output_PEAXAI_models <- list(
        best_model_fit = save_best_model_fit,
        performance_train = save_performance_train,
        performance_train_all = performance_train_all_by_dataset
      )

      return(output_PEAXAI_models)

    }

  }

  # It is necessary to determine the complete facets with ALL data
  get_imbalance <- function(df) as.numeric(sub("\\*$", "", as.character(df["imbalance_rate"])))

  best_imbalance <- sort(unique(unlist(lapply(save_performance_validation, get_imbalance))), na.last = NA)

  if (length(best_imbalance) == 1 & best_imbalance[1] == real_balance) {

    datasets_all <- list(all_data)
    real_balance <- round(prop.table(table(all_data$class_efficiency)), 4)
    names(datasets_all)[1] <- paste0(as.character(round(real_balance, 4)),"*")

  } else {

    if(round(real_balance, 4) %in% best_imbalance) {

      best_imbalance_SMOTE <- best_imbalance[which(round(real_balance, 4) != best_imbalance)]

      datasets_all <- list(all_data)
      real_balance <- round(prop.table(table(all_data$class_efficiency)), 4)[1]
      names(datasets_all)[1] <- paste0(as.character(round(real_balance, 4)),"*")

      if (as.character(RTS) %in% RTS_available) {

        # determine complete facets
        train_data_SMOTE <- SMOTE_data(
          data = all_data,
          x = x,
          y = y,
          RTS = RTS,
          balance_data = best_imbalance_SMOTE
        )

        datasets_all <- append(train_data_SMOTE, datasets_all, after = 0)

      }

    } else {

      datasets_all <- list()
      best_imbalance_SMOTE <- best_imbalance

      if (as.character(RTS) %in% RTS_available) {

        # determine complete facets
        train_data_SMOTE <- SMOTE_data(
          data = all_data,
          x = x,
          y = y,
          RTS = RTS,
          balance_data = best_imbalance_SMOTE
        )

        datasets_all <- append(train_data_SMOTE, datasets_all)

      }

    }

  }

  # train with ALL data: best balance + hypeparameters
  for (method_i in names(methods)) {

    imbalance_rate_i <- save_performance_validation[[method_i]]$imbalance_rate

    if (grepl("\\*$", imbalance_rate_i)) {
      imbalance_rate_i <- paste0(round(real_balance, 4), "*")
    }

    names_tuneGrid <- names(best_methods[[method_i]][["tuneGrid"]])

    if (method_i != "glm") {
      best_methods[[method_i]][["tuneGrid"]] <- save_performance_validation[[method_i]][names_tuneGrid]
    }

    model_fit <- train_PEAXAI(
      data = datasets_all[[as.character(imbalance_rate_i)]],
      method = method_i,
      parameters = best_methods[[method_i]],
      trControl = trainControl(method = "none", classProbs = TRUE),
      metric_priority = metric_priority
    )

    # actualize weights
    if (method_i == "glm") {

      parameters <- methods[["glm"]]

      if (parameters[["weights"]][1] == "dinamic") {
        w0 <- nrow(datasets_all[[as.character(imbalance_rate_i)]]) / (2 * length(which(datasets_all[[as.character(imbalance_rate_i)]]$class_efficiency == "not_efficient")))
        w1 <- nrow(datasets_all[[as.character(imbalance_rate_i)]]) / (2 * length(which(datasets_all[[as.character(imbalance_rate_i)]]$class_efficiency == "efficient")))
      } else if (is.data.frame(parameters[["weights"]])) {
        w0 <- parameters[["weights"]][["w0"]]
        w1 <- parameters[["weights"]][["w1"]]
      } else {
        w0 <- 1
        w1 <- 1
      }

      save_performance_validation[[method_i]]$w0 <- w0
      save_performance_validation[[method_i]]$w1 <- w1

    }

    save_best_model_fit[[method_i]] <- model_fit
  }

  output_PEAXAI_models <- list(
    best_model_fit = save_best_model_fit,
    performance_validation = save_performance_validation,
    performance_train = save_performance_train,
    performance_train_all = performance_train_all_by_dataset
  )

  return(output_PEAXAI_models)

}


  # end selecting best hyperparameters
#comentar ######################################################################
#
#   # save model and performance
#   best_models <- vector("list", length(methods))
#   names(best_models) <- names(methods)
#
#   best_performance  <- vector("list", length(methods))
#   names(best_performance) <- names(methods)
#
#   # extra: more information about fitting
#   performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
#   names(performance_train_all_by_dataset) <- names(datasets_to_train)
#
#   # only the best by imbalance and ML method
#   best_performance_train_all_by_dataset <- vector("list", length(datasets_to_train))
#   names(best_performance_train_all_by_dataset) <- names(datasets_to_train)
#
#   metric <- metric_priority[1]
#
#   for (dataset in names(datasets_to_train)) {
#
#     message(paste0("Case: ", dataset, " imbalance rate"))
#
#     if (trControl[["method"]] == "cv") {
#
#       # create k-folds. Same folds for every ML method
#       folds <- createFolds(
#         datasets_to_train[[dataset]]$class_efficiency,
#         k = trControl[["number"]])
#
#     } else if (trControl[["method"]] == "test_set") {
#
#       # 1 fold trin, 2 fold test. they have different nrows
#       folds <- vector("list", length = 2)
#       names(folds) <- c("Fold1", "Fold2")
#
#       # determine which samples are test
#       test_fold <- createDataPartition(
#         datasets_to_train[[dataset]]$class_efficiency,
#         p = trControl[["test_hold_out"]])
#
#       # save training and test sets
#       folds[["Fold1"]] <- test_fold$Resample1
#
#       folds[["Fold2"]] <- setdiff(1:nrow(datasets_to_train[[dataset]]), test_fold[[1]])
#
#     }
#
#     performance_train_all_by_method <- vector("list", length(methods))
#     names(performance_train_all_by_method) <- names(methods)
#
#     best_performance_train_all_by_method <- vector("list", length(methods))
#     names(best_performance_train_all_by_method) <- names(methods)
#
#     for (method_i in names(methods)) {
#
#       message(paste0("Training ", method_i, " method."))
#
#       arguments <- methods[[method_i]]
#
#       # save best model if the new if better
#       best_record <- NULL
#
#       # save performance by tuneGrid
#       performance_by_tuneGrid <- NULL
#
#       # information_performance <- NULL
#       if(method_i == "glm") {
#         arguments[["tuneGrid"]] <- data.frame(1)
#       }
#
#       for (hyparameter_i in 1:nrow(arguments[["tuneGrid"]])) {
#
#         hyparameter <- arguments[["tuneGrid"]][hyparameter_i, ]
#         hyparameter <- as.data.frame(hyparameter)
#         names(hyparameter) <- names(arguments[["tuneGrid"]])
#
#         # params to predict
#         if (method_i == "glm") {
#           type <- "response"
#           levels <- c(0,1)
#         } else {
#           type <- "prob"
#           levels <- c("efficient", "not_efficient")
#         }
#
#         # for each fold, save performance. After, calculate the mean
#         performance_by_fold <- NULL
#         iter <- 0
#
#         for (fold in folds) {
#
#           iter <- iter + 1
#           if (trControl[["method"]] == "none" & iter == 2) {
#             next
#           }
#
#           training_fold <- datasets_to_train[[dataset]][-fold, ]
#           test_fold <- datasets_to_train[[dataset]][fold, ]
#
#           if (method_i == "nnet") {
#
#             model_fit <- train(
#               class_efficiency ~ .,
#               data = training_fold,
#               method = "nnet",
#               preProcess = arguments[["preProcess"]],
#               tuneGrid = hyparameter,
#               trControl = trainControl(method = "none", classProbs = TRUE),
#               metric = metric,
#
#               # nnet (no fine-tuning)
#               skip = arguments[["skip"]],
#               maxit = arguments[["maxit"]],
#               MaxNWts = arguments[["MaxNWts"]],
#               trace = arguments[["trace"]]
#             )
#
#           } else if (method_i == "rf") {
#
#
#             # model_fit <- train(
#             #   class_efficiency ~ .,
#             #   data = training_fold,
#             #   method = "rf",
#             #   tuneGrid = hyparameter,
#             #   trControl = trainControl(method = "none", classProbs = TRUE),
#             #   metric = metric,
#             #
#             #   # rf (no fine-tuning)
#             #   ntree = arguments[["ntree"]]
#             # )
#
#           } else if (method_i == "svmPoly") {
#
#             model_fit <- train(
#               class_efficiency ~ .,
#               data = training_fold,
#               method = "svmPoly",
#               tuneGrid = hyparameter,
#               trControl = trainControl(method = "none", classProbs = TRUE, verboseIter = FALSE),
#               metric = metric,
#               prob.model = TRUE,
#               )
#               # preProcess = c("center","scale"))
#
#           } else if (method_i == "glm") {
#
#             if (iter == 1) {
#               save_weights <- arguments[["weights"]]
#             } else {
#               arguments[["weights"]] <- save_weights
#             }
#
#             if (is.factor(training_fold$class_efficiency)) {
#               training_fold$class_efficiency <- ifelse(training_fold$class_efficiency == "efficient", 1, 0)
#             }
#
#             # Is there weights?
#             if(!is.null(arguments[["weights"]])) {
#
#               if (arguments[["weights"]][1] == "dinamic") {
#
#                 # original_weights <- arguments[["weights"]]
#
#                 w0 <- nrow(training_fold) / (2 * length(which(training_fold$class_efficiency == 0)))
#                 w1 <- nrow(training_fold) / (2 * length(which(training_fold$class_efficiency == 1)))
#
#                 weights <-  ifelse(training_fold$class_efficiency == 1, w1, w0)
#
#                 # save weights
#                 arguments[["weights"]] <- weights
#
#               } else {
#
#                 weights <-  ifelse(
#                   training_fold$class_efficiency == 1,
#                   arguments[["weights"]][["w1"]], arguments[["weights"]][["w0"]]
#                   )
#
#                 # save weights
#                 arguments[["weights"]] <- weights
#
#               }
#             }
#
#             model_fit <- glm(
#               class_efficiency ~.,
#               data = training_fold,
#               family = arguments[["family"]],
#               weights = arguments[["weights"]])
#
#             # summary(model_fit)
#
#             model_fit <- step(
#               model_fit,
#               direction = arguments[["direction"]],
#               trace = arguments[["trace"]])
#             # summary(model_fit)
#
#           }
#
#       # ------------------------------------------------------------------------
#       # test performance -------------------------------------------------------
#       # ------------------------------------------------------------------------
#
#           # change to glm
#           if (method_i == "glm") {
#             test_fold$class_efficiency <- ifelse(test_fold$class_efficiency == "efficient", 1, 0)
#             y_hat <- predict(model_fit, newdata = test_fold[, setdiff(names(test_fold), "class_efficiency")], type = type)
#           } else {
#             # predicted data
#             y_hat <- predict(model_fit, newdata = test_fold[, setdiff(names(test_fold), "class_efficiency")], type = type)[,1]
#           }
#           if(any(is.na(y_hat))) {
#             next
#             browser()
#           }
#
#           y_hat_prob <- y_hat
#
#           if (method_i == "glm") {
#
#             # Logistic regression: 1 not_efficient level, 2 efficient level
#             y_hat <- ifelse(y_hat > 0.5, levels[2], levels[1])
#
#             # observed data
#             y_obs  <- factor(
#               test_fold$class_efficiency,
#               levels = levels
#             )
#
#           } else {
#
#             # ML general: 1 efficient level, 2 not_efficient level
#             y_hat <- ifelse(y_hat > 0.5, levels[1], levels[2])
#
#             # observed data
#             y_obs  <- test_fold$class_efficiency
#
#           }
#
#           # change to factor
#           y_hat <- factor(
#             y_hat,
#             levels = levels)
#
#           if (method_i == "glm") {
#             cm <- confusionMatrix(
#               data = y_hat,
#               reference = y_obs,
#               mode = "everything",
#               positive = "1"
#             )
#           } else {
#             cm <- confusionMatrix(
#               data = y_hat,
#               reference = y_obs,
#               mode = "everything",
#               positive = "efficient"
#             )
#           }
#
#           if (!(method_i == "glm")) {
#             # first the negative level
#             levels <- rev(levels)
#           }
#
#           # ROC-AUC
#           roc_obj <- pROC::roc(
#             response = y_obs,
#             predictor = y_hat_prob,
#             levels = levels,
#             direction = "<",
#             quiet = TRUE)
#
#           if (!method_i == "glm") {
#             # again the origina levels
#             levels <- rev(levels)
#           }
#
#           if (method_i == "glm") {
#             first_eff <- 2
#             second_not_eff <- 1
#
#           } else {
#             first_eff <- 1
#             second_not_eff <- 2
#           }
#
#           # PR-AUC
#           pr_obj <- PRROC::pr.curve(
#             scores.class0 = y_hat_prob[y_obs == levels[second_not_eff]],
#             scores.class1 = y_hat_prob[y_obs == levels[first_eff]],
#             curve = TRUE
#           )
#
#           out <- c(
#             cm$overall[c("Accuracy", "Kappa")],
#             cm$byClass[c("Recall", "Specificity",
#                          "Precision", "F1",
#                          "Balanced Accuracy")],
#             "ROC-AUC" = roc_obj$auc,
#             "PR-AUC" = unname(pr_obj$auc.integral)
#           )
#
#           # not NAs, use all folds to colmeans
#           if (is.na(out[["F1"]])) {
#             out[["F1"]] <- 0
#             out[["Precision"]] <- 0
#           }
#
#           if (is.na(out[["Precision"]])) {
#             out[["Precision"]] <- 0
#           }
#
#           # save prformance
#           performance_by_fold <- rbind(performance_by_fold, out)
#
#         } # end performance fold
#         # if(is.null(performance_by_fold)) browser()
#         if (!is.null(performance_by_fold)) {
#           # mean of fold performance
#           performance <- colMeans(performance_by_fold)
#         } else {
#           performance <- t(as.data.frame(matrix(
#             data = NA,
#             ncol = 9,
#             nrow = 1
#           )))
#         }
#
#         performance <- t(as.data.frame(performance))
#         colnames(performance) <- names(out)
#         row.names(performance) <- NULL
#
#
#         df_hyp <- data.frame(
#           data = hyparameter
#         )
#
#         # tolerance
#         eps <- 0.00001
#
#         if (method_i == "glm") {
#           performance <- performance
#
#         } else {
#
#           names(df_hyp) <- names(arguments[["tuneGrid"]])
#
#           performance <- cbind(df_hyp, performance)
#
#         }
#
#         # save if the new combination hyperparameters is better
#         if (is.null(best_record)) {
#
#           best_record <- performance
#
#         } else {
#
#           if (!is.na(performance[[metric]])) {
#
#             if (performance[[metric]] > best_record[[metric]]) {
#
#               best_record <- performance
#
#             } else if (abs(performance[[metric]] - best_record[[metric]]) <= eps) {
#
#               for (m in metric_priority[-1]) {
#
#                 result <- abs(performance[[m]] - best_record[[m]])
#
#                 if (result > eps) {
#
#                   best_record <- performance
#                   # end comprobation
#                   break
#                 }
#
#               }
#
#             }
#
#           }
#
#         }
#
#         performance_by_tuneGrid <- rbind(performance_by_tuneGrid, performance)
#
#       } # end tuneGrid
#
#       # save the training results
#       performance_train_all_by_method[[method_i]] <- performance_by_tuneGrid
#
#       best_performance_train_all_by_method[[method_i]] <- best_record
#
#     }
#
#     performance_train_all_by_dataset[[dataset]] <- performance_train_all_by_method
#     best_performance_train_all_by_dataset[[dataset]] <- best_performance_train_all_by_method
#
#   }
#
#     # --------------------------------------------------------------------------
#     # Step 3.3: Train the best models (without cross-validation) ---------------
#     # --------------------------------------------------------------------------
#
#   # get the best in each imbalance rate
#   get_model <- function(lst, model = names(methods)) {
#     model <- match.arg(model)
#     out <- lapply(names(lst), function(k) {
#       x <- lst[[k]][[model]]
#       if (is.null(x)) return(NULL)
#       cbind(model = model, imbalance = k, x, row.names = NULL)
#     })
#     do.call(rbind, out)
#   }
#
#   # train the best models without cross-validation
#   save_best_performance <- vector("list", length = length(methods))
#   names(save_best_performance) <- names(methods)
#
#   for (method_i in names(methods)) {
#
#     #a dataframe with results
#     best_results_train <- get_model(best_performance_train_all_by_dataset, method_i)
#
#     best_results_train <- as.data.frame(best_results_train)
#
#     best_results_train$imbalance <- as.numeric(gsub("\\*", "", best_results_train$imbalance))
#
#     if (method_i == "glm") {
#
#       best_results_train$imbalance <- round(best_results_train$imbalance, 4)
#
#       best_results_train$diff_real_balance[1] <- real_balance
#
#     } else {
#
#       best_results_train$diff_real_balance <- as.numeric(best_results_train$imbalance)
#
#       best_results_train$diff_real_balance[1] <- real_balance
#
#     }
#
#     # distance to real balance
#     best_results_train$diff_real_balance <- round(abs(best_results_train$imbalance - real_balance), 4)
#
#     if (method_i == "glm") {
#
#       arguments[["tuneGrid"]] <- data.frame(1)
#
#       best_results_train <- best_results_train %>%
#         arrange(desc(.data[[metric]]), diff_real_balance)
#
#     } else {
#
#       names_hyparameters <- names(methods[[method_i]][["tuneGrid"]])
#
#       best_results_train <- best_results_train %>%
#         arrange(desc(.data[[metric]]), diff_real_balance)
#
#       best_hyparameters <- best_results_train[1, names_hyparameters]
#       best_hyparameters <- as.data.frame(best_hyparameters)
#       names(best_hyparameters) <- names_hyparameters
#
#     }
#
#     best_balance <- best_results_train[1, "imbalance"]
#
#     # change to original name
#     if (best_balance == round(real_balance, 4)) {
#       best_balance <- paste0(best_balance,"*")
#     }
#     best_balance <- as.character(best_balance)
#
#     arguments <- methods[[method_i]]
#
#     # predict output
#     if (method_i == "glm") {
#
#       type <- "response"
#       levels <- c(0,1)
#
#     } else {
#
#       type <- "prob"
#       levels <- c("efficient", "not_efficient")
#
#     }
#
#     # train the method
#     if (method_i == "nnet") {
#
#       model_fit <- train(
#         class_efficiency ~ .,
#         data = datasets_to_train[[best_balance]],
#         method = "nnet",
#         preProcess = arguments[["preProcess"]],
#         tuneGrid = best_hyparameters,
#         trControl = trainControl(method = "none", classProbs = TRUE),
#         metric = metric,
#
#         # nnet (no fine-tuning)
#         skip = arguments[["skip"]],
#         maxit = arguments[["maxit"]],
#         MaxNWts = arguments[["MaxNWts"]],
#         trace = arguments[["trace"]]
#       )
#
#     } else if (method_i == "rf") {
#
#       # model_fit <- train(
#       #   class_efficiency ~ .,
#       #   data = datasets_to_train[[best_balance]],
#       #   method = "rf",
#       #   tuneGrid = best_hyparameters,
#       #   trControl = trainControl(method = "none", classProbs = TRUE),
#       #   metric = metric,
#       #
#       #   # rf (no fine-tuning)
#       #   ntree = arguments[["ntree"]]
#       # )
#
#     } else if (method_i == "svmPoly") {
#
#       model_fit <- train(
#         class_efficiency ~ .,
#         data = datasets_to_train[[best_balance]],
#         method = "svmPoly",
#         tuneGrid = best_hyparameters,
#         trControl = trainControl(method = "none", classProbs = TRUE),
#         metric = metric)
#       # preProcess = c("center","scale"))
#
#     } else if (method_i == "glm") {
#
#       if (is.factor(datasets_to_train[[best_balance]][["class_efficiency"]])) {
#         datasets_to_train[[best_balance]]$class_efficiency <- ifelse(datasets_to_train[[best_balance]]$class_efficiency == "efficient", 1, 0)
#       }
#
#       # Is there weights?
#       if(!is.null(arguments[["weights"]])) {
#         if (arguments[["weights"]] == "dinamic") {
#
#           # original_weights <- arguments[["weights"]]
#
#           w0 <- nrow(datasets_to_train[[best_balance]]) / (2 * length(which(datasets_to_train[[best_balance]]$class_efficiency == 0)))
#           w1 <- nrow(datasets_to_train[[best_balance]]) / (2 * length(which(datasets_to_train[[best_balance]]$class_efficiency == 1)))
#
#           weights <- ifelse(datasets_to_train[[best_balance]]$class_efficiency == 1, w1, w0)
#
#           # save weights
#           arguments[["weights"]] <- weights
#
#         }
#       }
#
#       model_fit <- glm(
#         class_efficiency ~.,
#         data = datasets_to_train[[best_balance]],
#         family = arguments[["family"]],
#         weights = arguments[["weights"]])
#
#       model_fit <- step(model_fit, direction = arguments[["direction"]], trace = arguments[["trace"]])
#
#     }
#
#       # ------------------------------------------------------------------------
#       # Validation dataset check -----------------------------------------------
#       # ------------------------------------------------------------------------
#
#     # change to glm
#     if (method_i == "glm") {
#       valid_data$class_efficiency <- ifelse(valid_data$class_efficiency == "efficient", 1, 0)
#       y_hat <- predict(model_fit, newdata = valid_data[, setdiff(names(valid_data), "class_efficiency")], type = type)
#
#     } else {
#       # predicted data
#       y_hat <- predict(model_fit, newdata = valid_data[, setdiff(names(valid_data), "class_efficiency")], type = type)[,1]
#     }
#
#     y_hat_prob <- y_hat
#
#     if (method_i == "glm") {
#
#       # Logistic regression: 1 not_efficient level, 2 efficient level
#       y_hat <- ifelse(y_hat > 0.5, levels[2], levels[1])
#
#       # observed data
#       y_obs  <- factor(
#         valid_data$class_efficiency,
#         levels = levels
#       )
#
#     } else {
#
#       # ML general: 1 efficient level, 2 not_efficient level
#       y_hat <- ifelse(y_hat > 0.5, levels[1], levels[2])
#
#       # observed data
#       y_obs  <- valid_data$class_efficiency
#
#     }
#
#     # change to factor
#     y_hat <- factor(
#       y_hat,
#       levels = levels)
#
#     if (method_i == "glm") {
#       cm <- confusionMatrix(
#         data = y_hat,
#         reference = y_obs,
#         mode = "everything",
#         positive = "1"
#       )
#     } else {
#       cm <- confusionMatrix(
#         data = y_hat,
#         reference = y_obs,
#         mode = "everything",
#         positive = "efficient"
#       )
#     }
#
#     if (!method_i == "glm") {
#       # first the negative level
#       levels <- rev(levels)
#     }
#
#     # ROC-AUC
#     roc_obj <- pROC::roc(
#       response = y_obs,
#       predictor = y_hat_prob,
#       levels = levels,
#       direction = "<",
#       quiet = TRUE)
#
#     if (!method_i == "glm") {
#       # again the origina levels
#       levels <- rev(levels)
#     }
#
#     if (method_i == "glm") {
#       first_eff <- 2
#       second_not_eff <- 1
#
#     } else {
#       first_eff <- 1
#       second_not_eff <- 2
#     }
#
#     # PR-AUC
#     pr_obj <- PRROC::pr.curve(
#       scores.class0 = y_hat_prob[y_obs == levels[second_not_eff]],
#       scores.class1 = y_hat_prob[y_obs == levels[first_eff]],
#       curve = TRUE
#     )
#
#     out <- c(
#       cm$overall[c("Accuracy", "Kappa")],
#       cm$byClass[c("Recall", "Specificity",
#                    "Precision", "F1",
#                    "Balanced Accuracy")],
#       "ROC-AUC" = roc_obj$auc,
#       "PR-AUC" = unname(pr_obj$auc.integral)
#     )
#
#     # save performance in validation set
#     best_balance <- data.frame(
#       "method" = method_i,
#       "best_balance" = best_balance
#     )
#
#     if (method_i == "glm") {
#
#       if (!is.null(arguments[["weights"]])) {
#
#         best_hyparameters <- data.frame(
#           w0 = round(w0, 4),
#           w1 = round(w1, 4)
#         )
#
#         # change direction columns
#         out <- t(out)
#
#         save_best_performance[[method_i]] <- cbind(best_balance, best_hyparameters, out)
#
#       } else {
#         out <- t(out)
#         save_best_performance[[method_i]] <- cbind(best_balance, out)
#
#       }
#
#     } else {
#
#       best_hyparameters <-  data.frame(
#         best_hyparameters
#       )
#       names(best_hyparameters) <- names(arguments[["tuneGrid"]])
#
#       # change direction columns
#       out <- t(out)
#
#       df_per <- cbind(best_balance, best_hyparameters, out)
#
#       if (nrow(df_per) > 1) {
#         t(df_per)
#       }
#
#       save_best_performance[[method_i]] <- df_per
#
#     }
#
#   }
#
#   # ----------------------------------------------------------------------------
#   # Step 4: Train best models (all data + balance) -----------------------------
#   # ----------------------------------------------------------------------------
#
#   extract_imbalance <- function(df) {
#     # busca nombre de columna plausible
#     cand <- c("best_balance")
#     col <- intersect(cand, names(df))
#     if (length(col) == 0) return(NA_real_)
#     vals <- unique(df[[col[1]]])
#   }
#
#   imbal_vec <- sapply(save_best_performance, extract_imbalance)
#   imbal_vec <- imbal_vec[!is.na(as.numeric(imbal_vec))]
#   new_balance_data <- imbal_vec
#
#   if (is.null(imbalance_rate)) {
#
#     all_data_SMOTE <- all_data
#
#   } else {
#
#     if (length(new_balance_data) != 0) {
#       # the best dataset is always the original
#
#       all_data_SMOTE <- SMOTE_data(
#         data = all_data,
#         x = x,
#         y = y,
#         RTS = RTS,
#         balance_data = unique(as.numeric(new_balance_data))
#       )
#
#     }
#
#   }
#
#   # if is non balance dataset needed?
#   extract_imbalance <- function(df) {
#     # busca nombre de columna plausible
#     cand <- c("best_balance")
#     col <- intersect(cand, names(df))
#     if (length(col) == 0) return(NA_real_)
#     vals <- unique(df[[col[1]]])
#     # limpia a numérico (quita asteriscos u otros símbolos)
#     as.numeric(gsub("[^0-9.]+", "", as.character(vals)))
#   }
#
#   imbal_vec <- sapply(save_best_performance, extract_imbalance)
#
#   # safe to change after
#   old_real_balance <- round(real_balance, 4)
#
#   if (round(real_balance, 4) %in% imbal_vec) {
#
#     if (all(imbal_vec == round(real_balance, 4))) {
#       all_data_SMOTE <- list(all_data)
#       names(all_data_SMOTE) <- paste0(as.character(round(real_balance, 4)), "*")
#     } else {
#       # add no balance dataset
#       all_data_SMOTE <- append(all_data_SMOTE, list(all_data), after = 0)
#       real_balance <- prop.table(table(all_data$class_efficiency))[1]
#       real_balance <- round(real_balance, 4)
#       names(all_data_SMOTE)[1] <- paste0(as.character(round(real_balance, 4),"*"))
#
#       names(all_data_SMOTE)
#     }
#
#   }
#
#   save_best_models <- vector("list", length = length(names(methods)))
#   names(save_best_models) <- names(methods)
#
#   for (method_i in names(methods)) {
#
#     # select the balance dataset in
#     best_imbalance_i <- save_best_performance[[method_i]][["best_balance"]]
# if(length(best_imbalance_i) > 1) browser()
#     if (best_imbalance_i == paste0(as.character(old_real_balance),"*")) {
#
#       best_imbalance_i <- paste0(as.character(round(real_balance, 4)),"*")
#       save_best_performance[[method_i]][["best_balance"]] <- best_imbalance_i
#     }
#
#     if (method_i == "glm") {
#
#       arguments[["tuneGrid"]] <- data.frame(1)
#
#       # select hyparameters
#       names_hyparameters <- c("w0", "w1")
#
#     } else {
#
#       # select hyparameters
#       names_hyparameters <- names(methods[[method_i]][["tuneGrid"]])
#
#     }
#
#     arguments <- methods[[method_i]]
#
#
#     if (method_i == "glm" & is.null(arguments[["weights"]])) {
#
#       best_hyparameters <- NULL
#
#     } else {
#
#       best_hyparameters <- save_best_performance[[method_i]][, names_hyparameters]
#
#       best_hyparameters <- as.data.frame(
#         best_hyparameters
#       )
#       names(best_hyparameters) <- names_hyparameters
#     }
#
#     # train the method
#     if (method_i == "nnet") {
#
#       model_fit <- train(
#         class_efficiency ~ .,
#         data = all_data_SMOTE[[best_imbalance_i]],
#         method = "nnet",
#         preProcess = arguments[["preProcess"]],
#         tuneGrid = best_hyparameters,
#         trControl = trainControl(method = "none", classProbs = TRUE),
#         metric = metric,
#
#         # nnet (no fine-tuning)
#         skip = arguments[["skip"]],
#         maxit = arguments[["maxit"]],
#         MaxNWts = arguments[["MaxNWts"]],
#         trace = arguments[["trace"]]
#       )
#
#     } else if (method_i == "rf") {
#
#       model_fit <- train(
#         class_efficiency ~ .,
#         data = all_data_SMOTE[[best_imbalance_i]],
#         method = "rf",
#         tuneGrid = best_hyparameters,
#         trControl = trainControl(method = "none", classProbs = TRUE),
#         metric = metric,
#
#         # rf (no fine-tuning)
#         ntree = arguments[["ntree"]]
#       )
#
#     } else if (method_i == "svmPoly") {
#
#       model_fit <- train(
#         class_efficiency ~ .,
#         data = all_data_SMOTE[[best_imbalance_i]],
#         method = "svmPoly",
#         tuneGrid = best_hyparameters,
#         trControl = trainControl(method = "none", classProbs = TRUE),
#         metric = metric)
#       # preProcess = c("center","scale"))
#
#     } else if (method_i == "glm") {
#
#       if (is.factor(all_data_SMOTE[[best_imbalance_i]][["class_efficiency"]])) {
#         all_data_SMOTE[[best_imbalance_i]]$class_efficiency <- ifelse(all_data_SMOTE[[best_imbalance_i]]$class_efficiency == "efficient", 1, 0)
#       }
#
#       # Is there weights?
#       if(!is.null(arguments[["weights"]])) {
#         if (arguments[["weights"]] == "dinamic") {
#
#           weights <- save_best_performance[[method_i]][, c("w0", "w1")]
#
#           weights <- ifelse(all_data_SMOTE[[best_imbalance_i]]$class_efficiency == 1, weights[["w1"]], weights[["w0"]])
#
#           # save weights
#           arguments[["weights"]] <- weights
#
#
#         }
#       } else {
#
#         weights <- NULL
#         # save weights
#         arguments[["weights"]] <- weights
#
#       }
#
#       model_fit <- glm(
#         class_efficiency ~.,
#         data = all_data_SMOTE[[best_imbalance_i]],
#         family = arguments[["family"]],
#         weights = arguments[["weights"]])
#
#       model_fit <- step(model_fit, direction = arguments[["direction"]], trace = arguments[["trace"]])
#
#     }
#
#     model_fit$results <- save_best_performance[[method_i]]
#     save_best_models[[method_i]] <- model_fit
#
#   }
#
#   # message with the best
#   df_performance <- NULL
#
#   for (a in names(methods)) {
#
#     idx <- 1:length(names(methods))
#     idx_i <- idx[which(names(methods) == a)]
#
#     # models <- save_best_performance[[a]]
#
#     # save parameters
#     names_out <- as.data.frame(out)
#     names_out <- names(names_out)
#
#     performance <- save_best_performance[[a]][, names_out]
#
#     name_method <- as.data.frame(a)
#     names(name_method) <- "method"
#     names_out <- cbind(name_method, performance)
#
#     if (is.null(df_performance)) {
#
#       df_performance <- names_out
#
#     } else {
#
#       df_performance <- rbind(df_performance, names_out)
#
#     }
#
#   }
#
#   df_performance <- df_performance %>%
#     dplyr::arrange(dplyr::desc(.data[[metric]]), is.na(.data[[metric]]))
#
#
#   message(paste0("The ML method which has the best performance on ", metric, " was ", df_performance[1,1]))
#
#   output_PEAXAI <- list(
#     best_model_name = df_performance[1,1],
#     best_models = save_best_models,
#     best_performance = save_best_performance,
#     train_performace_informtation = performance_train_all_by_dataset
#   )
#
#   return(output_PEAXAI)
#
# }
