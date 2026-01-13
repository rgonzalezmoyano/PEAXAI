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
#' @param z_numeric Integer vector with column indices of numeric environment variables in \code{data}. By default is \code{NULL}.
#' @param z_factor Integer vector with column indices of factor environment variables in \code{data}. By default is \code{NULL}.
#' @param B number of bootstrap replicates in Conditional DEA.
#' @param m number of units to be included in the reference set.
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
#' @param metric_priority A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_Accuracy"} due to (normally) unbalanced data.
#' @param calibration_method Character string specifying the probability calibration
#'   method applied to the fitted classifier. Supported options are:
#'   \describe{
#'     \item{\code{"none"}}{No probability calibration is applied; raw classifier
#'       probabilities are returned.}
#'     \item{\code{"platt"}}{Platt scaling, which fits a logistic regression model
#'       mapping raw scores to calibrated probabilities. Suitable for relatively
#'       small samples and parametric probability adjustment.}
#'     \item{\code{"isotonic"}}{Isotonic regression, a non-parametric monotonic
#'       calibration method that adapts flexibly to the empirical score-probability
#'       relationship. Recommended for larger samples where sufficient calibration
#'       data are available.}
#'   }
#'   Calibration is performed using validation data whenever resampling is enabled.
#' @param hold_out Numeric proportion in (0,1) for validation split (default \code{NULL}).
#'   If \code{NULL}, training and validation use the same data.
#' @param imbalance_rate Optional target(s) for class balance via SMOTE. If \code{NULL},
#'   no synthetic balancing is performed.
#' @param seed  Integer. Seed for reproducibility.
#' @param verbose Logical; if \code{TRUE}, prints progress messages (default \code{FALSE}).
#'
#' @importFrom caret train createDataPartition confusionMatrix createFolds
#' @importFrom dplyr %>% arrange across filter select summarise group_by first semi_join all_of bind_rows
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#' @importFrom stats binomial family median na.omit runif setNames approxfun
#' @importFrom utils combn tail
#' @importFrom rms val.prob
#' @importFrom isotone gpava
#' @importFrom np npudensbw
#'
#' @return A \code{"PEAXAI"} (list) with the best technique, best fitted models and their performance and the results by fold.
#'
#' @examples
#' \donttest{
#'   data("firms", package = "PEAXAI")
#'
#'   data <- subset(
#'     firms,
#'     autonomous_community == "Comunidad Valenciana"
#'   )
#'
#'   trControl <- list(
#'     method = "cv",
#'     number = 3
#'   )
#'
#'   # glm method
#'   methods <- list(
#'     "glm" = list(
#'         weights = "dinamic"
#'      )
#'   )
#'
#'   models <- PEAXAI_fitting(
#'     data = data,
#'     x = c(1:4),
#'     y = 5,
#'     RTS = "vrs",
#'     imbalance_rate = NULL,
#'     methods = methods,
#'     trControl = trControl,
#'     metric_priority = c("Balanced_Accuracy", "Precision"),
#'     calibration_method = "platt",
#'     seed = 1,
#'     verbose = FALSE
#'   )
#' }
#'
#' @export

PEAXAI_fitting <- function (
    data, x, y, z_numeric = NULL, z_factor = NULL, RTS = "vrs",
    B = NULL, alpha = FALSE, m = NULL, imbalance_rate = NULL,
    trControl, methods, metric_priority = "Balanced_Accuracy",
    calibration_method = NULL, hold_out = NULL, seed = NULL, verbose = TRUE
    ) {

  # ----------------------------------------------------------------------------
  # pre-processing -------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # check if parameters are well introduced
  validate_parametes_PEAXAI_fitting(
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

  # calibration parameters
  if (is.null(calibration_method)) {
    null_calibration <- TRUE
    calibration_method <- "platt"
  } else {
    null_calibration <- FALSE
  }

  mth_cal <- calibration_method
  alpha <- 0.05

  # seed
  if (is.null(seed)) {
    seed <- 314
  }

  # # reorder index 'x' and 'y' in data
  data_x <- data[,c(x), drop = FALSE]
  data_y <- data[,c(y), drop = FALSE]
  data_z_numeric <- data[, c(z_numeric), drop = FALSE]
  data_z_factor <- data[, c(z_factor), drop = FALSE]

  x <- 1:NCOL(data_x)
  y <- 1:NCOL(data_y)

  if(!is.null(z_numeric)) {
    z_numeric <- 1:NCOL(data_z_numeric)
  }

  if (!is.null(z_factor)) {
    z_factor <- 1:NCOL(data_z_factor)
  }

  y <- max(x) + y

  data <- cbind(data_x, data_y)

  # Check data
  data <- preprocessing(
    data = data,
    x = x,
    y = y
  )

  if (!is.null(z_numeric)) {
    # index
    z_numeric <- NCOL(data) + z_numeric

    # check
    ok <- all(vapply(data_z_numeric, is.numeric, logical(1)))
    if (!ok) {
      stop("All columns in z_numeric must be numeric.")
    }

    # add data
    data <- cbind(data, data_z_numeric)
  }

  if (!is.null(z_factor)) {
    # index
    z_factor <- NCOL(data) + z_factor

    # check
    ok <- all(vapply(data_z_factor, is.factor, logical(1)))
    if (!ok) {
      stop("All columns in z_factor must be factor.")
    }

    # add data
    data <- cbind(data, data_z_factor)
  }

  # save a copy before add class_efficiency
  copy_data_no_label <- data

  # copy of methods
  method_copy <- methods

  exogenous <- c(z_numeric, z_factor)

  # Similarity matrix of ALL dataset
  if (!is.null(exogenous)) {

    bandwidth <- npudensbw(dat = data[,exogenous])

  } else {
    bandwidth <- NULL
  }

  # ----------------------------------------------------------------------------
  # Step 1: Data labeling ------------------------------------------------------
  # ----------------------------------------------------------------------------

  data <- label_efficiency(
    data = data,
    x = x,
    y = y,
    z_numeric = z_numeric,
    z_factor = z_factor,
    RTS = RTS,
    B = B,
    m = m,
    alpha = alpha,
    bandwidth = bandwidth,
    seed = seed
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
      z_numeric = z_numeric,
      z_factor = z_factor,
      RTS = RTS,
      B = B,
      m = m,
      alpha = alpha,
      seed = seed
    )

    train_data <- label_efficiency(
      data = train_data,
      REF = train_data,
      x = x,
      y = y,
      z_numeric = z_numeric,
      z_factor = z_factor,
      RTS = RTS,
      B = B,
      m = m,
      alpha = alpha,
      seed = seed
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
  real_balance_stable <- paste0(as.character(round(real_balance, 4)),"*")

  # ----------------------------------------------------------------------------
  # Step 3.1: Addressing imbalance rate (if there is convexity) ----------------
  # ----------------------------------------------------------------------------
  RTS_available <- c("1", "vrs", "0", "crs")

  if (!is.null(imbalance_rate)) {

    if (as.character(RTS) %in% RTS_available) {

      if (is.null(z_numeric) & is.null(z_numeric)) {

        # create new datasets addressing imbalance
        train_data_SMOTE <- SMOTE_data(
          data = train_data,
          x = x,
          y = y,
          RTS = RTS,
          balance_data = imbalance_rate,
          seed = seed
        )

      } else {

        # create new datasets addressing imbalance
        train_data_SMOTE <- SMOTE_Z_data(
          data = train_data,
          x = x,
          y = y,
          z_numeric = z_numeric,
          z_factor = z_factor,
          balance_data = imbalance_rate,
          RTS = RTS,
          B = B,
          m = m,
          alpha = alpha,
          bandwidth = bandwidth,
          seed = seed
        )

      }

      datasets_to_train_FINAL <- append(train_data_SMOTE, datasets_to_train, after = 0)

    } else {
      message(
        "Without the convexity assumption, SMOTE units cannot be created on the efficient frontier."
      )

    }

  } else {

    datasets_to_train_FINAL <- datasets_to_train
  }

  # ----------------------------------------------------------------------------
  # Step 3.2: Performance with different hyperparameters -----------------------
  # ----------------------------------------------------------------------------
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

  # calibrating test
  # calibrating_datasets <- vector("list", length(datasets_to_train))
  # names(calibrating_datasets) <- names(datasets_to_train)

  save_calibrations_dataset <- vector("list", length(methods))
  names(save_calibrations_dataset) <- names(save_calibrations_dataset)

  # metrics evaluation
  metric <- metric_priority[1]

  # create k-folds. Same folds for every ML method
  folds <- createFolds(
    datasets_to_train[[1]]$class_efficiency,
    k = trControl[["number"]]
  )

  # performance by fold
  performance_by_fold <- vector("list", length(folds))
  names(performance_by_fold) <- names(folds)

  # save test set (data + indices) for each fold
  test_sets_by_fold <- vector("list", length(folds))
  names(test_sets_by_fold) <- names(folds)

  test_idx_by_fold <- vector("list", length(folds))
  names(test_idx_by_fold) <- names(folds)

  # calibration datasets (global; do NOT reinitialize inside folds)
  calibrating_datasets_by_method <- vector("list", length(methods))
  names(calibrating_datasets_by_method) <- names(methods)

  save_calibration_model <- vector("list", length(methods))
  names(save_calibration_model) <- names(methods)

  for(fold_i in names(folds)) {

    message(fold_i)

    test_set_idx <- unlist(folds[fold_i])
    train_set_idx <- unlist(folds[names(folds) != fold_i])

    test_set  <- train_data[test_set_idx, ]
    train_set <- train_data[train_set_idx, ]

    # store test info per fold
    test_sets_by_fold[[fold_i]] <- test_set
    test_idx_by_fold[[fold_i]]  <- test_set_idx

    # delete old label
    train_set <- train_set %>% select(-"class_efficiency")

    # re-label only on train
    train_set <- label_efficiency(
      data = train_set,
      REF = train_set,
      x = x,
      y = y,
      z_numeric = z_numeric,
      z_factor = z_factor,
      RTS = RTS,
      B = B,
      m = nrow(train_set)^(2/3),
      bandwidth = NULL,
      alpha = alpha,
      seed = seed
    )

    # --------------------------------------------------------------------------
    # Step 3.1: Addressing imbalance rate (if there is convexity) ---------------
    # --------------------------------------------------------------------------
    datasets_to_train <- list(train_set)
    names(datasets_to_train)[1] <- real_balance_stable

    RTS_available <- c("1", "vrs", "0", "crs")

    if (!is.null(imbalance_rate)) {

      if (as.character(RTS) %in% RTS_available) {

        if (is.null(z_numeric) & is.null(z_numeric)) {

          train_data_SMOTE <- SMOTE_data(
            data = train_set,
            x = x,
            y = y,
            RTS = RTS,
            balance_data = imbalance_rate,
            seed = seed
          )

        } else {

          # create new datasets addressing imbalance
          train_data_SMOTE <- SMOTE_Z_data(
            data = train_set,
            x = x,
            y = y,
            z_numeric = z_numeric,
            z_factor = z_factor,
            balance_data = imbalance_rate,
            RTS = RTS,
            B = B,
            m = m,
            alpha = alpha,
            bandwidth = NULL,
            seed = seed
          )

        }

        datasets_to_train <- append(train_data_SMOTE, datasets_to_train, after = 0)

      } else {
        message("Without the convexity assumption, SMOTE units cannot be created on the efficient frontier.")
      }
    } # end balancing

    # Train and get performance
    performance_train_all_by_method <- vector("list", length(methods))
    names(performance_train_all_by_method) <- names(methods)

    best_performance_train_all_by_method <- vector("list", length(methods))
    names(best_performance_train_all_by_method) <- names(methods)

    performance_by_fold_by_grid <- vector("list", length(methods))
    names(performance_by_fold_by_grid) <- names(methods)

    for (method_i in names(methods)) {

      if (isTRUE(verbose)) message(paste0("Training ", method_i, " method."))

      for (datasets_to_train_i in names(datasets_to_train)) {

        if(method_i == "glm") {

          methods[[method_i]][["tuneGrid"]] <- data.frame(1)
        }

        for (grid_i in 1:nrow(methods[[method_i]][["tuneGrid"]])) {

          new_parameters <- methods[[method_i]]

          if (is.data.frame(new_parameters[["tuneGrid"]])) {
            new_parameters[["tuneGrid"]] <- new_parameters[["tuneGrid"]][grid_i, ]
          } else {
            new_parameters[["tuneGrid"]] <- new_parameters[["tuneGrid"]][grid_i]
            new_parameters[["tuneGrid"]] <- as.data.frame(new_parameters[["tuneGrid"]])
            names(new_parameters[["tuneGrid"]]) <- names(methods[[method_i]][["tuneGrid"]])
          }

          # train model (inner CV handled in train_PEAXAI through trControl)
          model_fit <- train_PEAXAI(
            data = datasets_to_train[[datasets_to_train_i]],
            method = method_i,
            parameters = new_parameters,
            trControl = trControl,
            metric_priority = metric_priority,
            seed = seed
          )

          # --------------------------------------------------------------------
          # performance --------------------------------------------------------
          # --------------------------------------------------------------------
          levls <- c("efficient", "not_efficient")

          # raw probabilities
          y_hat_prob <- predict(model_fit, newdata = test_set, type = "prob")[, 1]

          # classifications
          y_hat <- ifelse(y_hat_prob > 0.5, "efficient", "not_efficient")
          y_hat <- factor(y_hat, levels = levls)

          # reference
          y_obs <- factor(test_set[, "class_efficiency"], levels = levls)

          # save calibration test per fold/method/balance/grid
          calibration <- cbind(as.data.frame(y_obs), y_hat_prob)
          names(calibration) <- c("obs", "efficient")

          # check if Calibration is needed
          y_obs_label <- ifelse(y_obs == "efficient", 1,0)

          eps <- 1e-5
          y_hat_prob <- pmin(pmax(y_hat_prob, eps), 1 - eps)

          calibration$DMU <- test_set_idx

          # store calibration for this fold
          calibrating_datasets_by_method[[method_i]][[datasets_to_train_i]][[fold_i]][[grid_i]] <- calibration

          # confusion matrix
          cm <- confusionMatrix(
            data = y_hat,
            reference = y_obs,
            mode = "everything",
            positive = "efficient"
          )

          if (is.na(cm$byClass[["F1"]])) {
            cm$byClass[["F1"]] <- 0
          }

          if (is.na(cm$byClass[["Precision"]])) {
            cm$byClass[["Precision"]] <- 0
          }

          # ROC/PR (robust when 1 class)
          if (length(unique(y_obs)) == 1) {

            roc_auc_val <- 0
            pr_auc_val  <- 0

          } else {

            roc_obj <- roc(
              response  = y_obs,
              predictor = y_hat_prob,
              levels    = rev(levls),
              direction = "<",
              quiet     = TRUE
            )

            pr_obj <- pr.curve(
              scores.class0 = y_hat_prob[y_obs == "not_efficient"],
              scores.class1 = y_hat_prob[y_obs == "efficient"],
              curve = TRUE
            )

            roc_auc_val <- as.numeric(roc_obj$auc)
            pr_auc_val  <- unname(pr_obj$auc.integral)
          }

          # gmean
          gmean <- sqrt(cm$byClass["Recall"]*cm$byClass["Specificity"])

          # --------------------------------------------------------------------
          # cross-entropy ------------------------------------------------------
          # --------------------------------------------------------------------
          # cross start
          y_bin <- ifelse(y_obs == "efficient", 1, 0)
          p <- calibration$efficient
          eps <- 1e-15
          p_clipped <- pmin(pmax(p, eps), 1 - eps)

          cross_entropy <- -mean(
            y_bin * log(p_clipped) + (1 - y_bin) * log(1 - p_clipped)
          )

          # cross-entropy EFFICIENT
          idx_eff <- which(y_bin == 1)
          y_bin_eff <- y_bin[idx_eff]
          p <- calibration$efficient[idx_eff]
          eps <- 1e-15
          p_clipped <- pmin(pmax(p, eps), 1 - eps)

          cross_entropy_eff <- -mean(
            y_bin_eff * log(p_clipped) + (1 - y_bin_eff) * log(1 - p_clipped)
          )

          # cross-entropy NOT EFFICIENT
          idx_not_eff <- which(y_bin == 0)
          y_bin_not_eff <- y_bin[idx_not_eff]
          p <- calibration$efficient[idx_not_eff]
          eps <- 1e-15
          p_clipped <- pmin(pmax(p, eps), 1 - eps)

          cross_entropy_not_eff <- -mean(
            y_bin_not_eff * log(p_clipped) + (1 - y_bin_not_eff) * log(1 - p_clipped)
          )


          performance <- c(
            cm$overall[c("Accuracy", "Kappa")],
            cm$byClass[c("Recall", "Specificity",
                         "Precision", "F1",
                         "Balanced Accuracy")],
            "G_mean" = unname(gmean),
            "ROC_AUC" = roc_auc_val,
            "PR_AUC" = pr_auc_val,
            "Cross_Entropy" = cross_entropy,
            "Cross_Entropy_Efficient_class" = cross_entropy_eff,
            "Cross_Entropy_not_Efficient_class" = cross_entropy_not_eff
          )

          names(performance)[7] <- "Balanced_Accuracy"
          performance_metrics_names <- names(performance)

          performance <- cbind(
            data.frame(method = method_i),
            data.frame(Fold = fold_i),
            new_parameters[["tuneGrid"]],
            as.data.frame(as.list(performance))
          )

          performance_by_fold_by_grid[[method_i]][[datasets_to_train_i]][[grid_i]] <- performance

        } # end grid loop

      } # end datasets loop

    } # end methods loop

    performance_by_fold[[fold_i]] <- performance_by_fold_by_grid

  } # end folds loop

  # method
  names_methods <- names(methods)
  results_fit <- vector("list", length(methods))

  for (method_i in names(methods)) {

    performace_by_imbalance <- NULL

    for (balance_i in names(datasets_to_train)) {

      for (grid_i in 1:NROW(methods[[method_i]][["tuneGrid"]])) {

        performace_tunGrid <- methods[[method_i]][["tuneGrid"]]

        for (fold_i in names(performance_by_fold)) {

          # warning(performance_by_fold[[fold_i]][[method_i]][[balance_i]][[grid_i]])
          performance_i <- performance_by_fold[[fold_i]][[method_i]][[balance_i]][[grid_i]]
          performance_i <- performance_i[performance_metrics_names]

          results_fit[[method_i]][[balance_i]][[fold_i]] <- performance_i

        }

        if(!is.data.frame(performace_tunGrid)) {
          performace_tunGrid <- as.data.frame(performace_tunGrid)
          names(performace_tunGrid) <- names(methods[[method_i]][["tuneGrid"]])
        }

        performace_tunGrid <- performace_tunGrid[grid_i,]

        performance_fold <- bind_rows(results_fit[[method_i]][[balance_i]])

        mean_fold <-  performance_fold %>%
          summarise(
            # mean
            across(all_of(performance_metrics_names), ~mean(.x, na.rm = TRUE))
          )

        sd_fold <- performance_fold %>%
          summarise(
            # SD
            across(all_of(performance_metrics_names), ~sd(.x, na.rm = TRUE),
                   .names = "{col}SD")
          )

        performance <- cbind(performace_tunGrid, mean_fold, sd_fold)

        performance_train_all_by_method[[method_i]][[balance_i]][[grid_i]] <- performance

      } # end by  grid

      performance_train_all_by_method[[method_i]][[balance_i]]
      performance <- bind_rows(performance_train_all_by_method[[method_i]][[balance_i]])

      # performance_train_all_by_method[[method_i]][[balance_i]] <- performance
      df_imbalance <- as.data.frame(balance_i)
      names(df_imbalance) <- "Imbalance_rate"
      if (method_i == "glm") {

        performance <- performance[2:ncol(performance)]
        performance <- cbind(df_imbalance, performance)
      } else {
        names_methods_grid <- names(methods[[method_i]][["tuneGrid"]])
        df_parameters <- performance[, names_methods_grid]
        names_metrics <- setdiff(names(performance), names_methods_grid)
        df_metrics <- performance[, names_metrics]
        performance <- cbind(df_imbalance, df_parameters, round(df_metrics, 2))
      }

      performace_by_imbalance <- rbind(performace_by_imbalance, performance)

    } # end by balance

    performance_train_all_by_method[[method_i]] <- performace_by_imbalance

  }

  # ----------------------------------------------------------------------------
  # Step 3.3: Train the best model and assing its performance ------------------
  # ----------------------------------------------------------------------------
  # train the best models without cross-validation
  save_performance_train <- vector("list", length = length(methods))
  names(save_performance_train) <- names(methods)

  save_best_model_fit <- vector("list", length = length(methods))
  names(save_best_model_fit) <- names(methods)

  save_calibration_model <- vector("list", length(methods))
  names(save_calibration_model) <- names(methods)

  for (method_i in names(methods)) {

    # first, select the best imbalance rate
    # dataframe with results of CV of method_i

    best_results_train <- performance_train_all_by_method[[method_i]]

    best_results_train <- as.data.frame(best_results_train)

    best_results_train$Imbalance_rate <- as.numeric(gsub("\\*", "", best_results_train$Imbalance_rate))
    best_results_train$diff_imbalance <- abs(best_results_train$Imbalance_rate - round(real_balance, 4))

    # how similar is the dataset to original dataset
    best_results_train$diff_imbalance <- 1 - best_results_train$diff_imbalance

    # add * in the orginal dataset name
    best_results_train$Imbalance_rate[best_results_train$diff_imbalance == 1] <- paste0(best_results_train$Imbalance_rate[best_results_train$diff_imbalance == 1], "*")

    # add difference imbalance
    metric_priority <- c(metric_priority, "diff_imbalance")

    best_results_train <- best_results_train[do.call(
      order, c(best_results_train[metric_priority], list(decreasing = TRUE))), ]

    best_balance_i <- best_results_train[1, "Imbalance_rate"]

    # copy of methods
    best_methods <- methods

    # only the best hyperparameters
    names_tuneGrid <- names(best_methods[[method_i]][["tuneGrid"]])

    if (method_i != "glm") {
      best_methods[[method_i]][["tuneGrid"]] <- best_results_train[1, names_tuneGrid]

      # get idx of hyparameters
      tg <- method_copy[[method_i]][["tuneGrid"]]
      br <- best_results_train[1, names_tuneGrid, drop = FALSE]

      idx <- which(
        apply(tg[, names_tuneGrid, drop = FALSE], 1,
              function(r) all(r == unlist(br)))
      )

    } else {
      idx <- 1
    }

    # train the best model
    # NO VALIDATION SET
    # All data used, performance is obteined of cv results
    model_fit <- train_PEAXAI(
      data = datasets_to_train_FINAL[[as.character(best_balance_i)]],
      method = method_i,
      parameters = best_methods[[method_i]],
      trControl = trainControl(method = "none", classProbs = TRUE),
      metric_priority = metric_priority[1],
      seed = seed
    )

    performance <- best_results_train[1,]

    # save calibration
    calibration_dataset <- NULL
    for(fold_i in names(folds)) {

      calibration_dataset <- rbind(calibration_dataset, calibrating_datasets_by_method[[method_i]][[as.character(best_balance_i)]][[fold_i]][[idx]])
    }

    calibration_dataset$obs <- ifelse(calibration_dataset$obs == "efficient", 1,0)

    res <- val.prob(
      p = calibration_dataset$efficient,
      y = calibration_dataset$obs)

    # check if it is necessary calibrator model
    if (res["U:p"] < alpha | res["S:p"] < alpha | abs(res["Intercept"]) > 0.2 | res["Slope"] - 1 > 0.2) {

      # function to apply calibration
      predict_calibrated <- function(newdata, model_fit, calibration_model, mth_cal) {

        s_new <- predict(model_fit, newdata = newdata, type = "prob")[, "efficient"]

        if (mth_cal == "isotonic") {
          # calibration_model is a function: iso_fun
          return(calibration_model(s_new))
        }

        # platt (glm)
        return(
          predict(calibration_model, newdata = data.frame(s = s_new), type = "response")
        )
      }

      if (mth_cal == "isotonic") {

        # 1. Variable respuesta binaria (1 = efficient)
        y_prob <- calibration_dataset$obs

        # 2. Probabilidades "raw" del modelo en el conjunto de calibración
        s <- calibration_dataset$efficient

        # 3. Ajuste isotónico (monótono creciente)
        iso_fit <- gpava(z = s, y = y_prob)

        # 4. Creamos una función continua para cualquier valor de probabilidad
        iso_fun <- approxfun(iso_fit$x, iso_fit$yf, rule = 2)

        calibration_model <- iso_fun

        # predict_calibrated <- function(newdata, model_fit, model_cal) {
        #
        #   # Probabilidades originales del modelo
        #   s_new <- predict(model_fit,
        #                    newdata = newdata,
        #                    type = "prob")[, "efficient"]
        #
        #   # Probabilidades calibradas con regresión isotónica
        #   p_cal <- model_cal(s_new)
        #
        #   return(p_cal)
        # }

      } else {

        # 1. Variable respuesta binaria (1 = efficient)
        y_prob <- calibration_dataset$obs

        # 2. Probabilidades "raw" del modelo en el conjunto de calibración
        s <- calibration_dataset$efficient

        # Platt
        platt_mod <- glm(y_prob ~ s, family = binomial)
        summary(platt_mod)

        calibration_model <- platt_mod

        # # apply calibration
        # predict_calibrated <- function(newdata, model_fit, model_cal) {
        #
        #   s_new <- predict(model_fit, newdata = newdata, type = "prob")[, "efficient"]
        #
        #   # 2. Probabilidad calibrada con Platt
        #   p_cal <- predict(model_cal,
        #                    newdata = data.frame(s = s_new),
        #                    type = "response")
        #
        #   return(p_cal)
        # }

      }

      # check is calibration works
      # check calibration
      calibration_dataset_cal <- calibration_dataset

      calibration_dataset_cal$efficient <- predict_calibrated(
        newdata = data[calibration_dataset_cal$DMU,c(x,y,z_numeric,z_factor)],
        model_fit = model_fit,
        calibration_model = calibration_model,
        mth_cal = mth_cal)

      res_2 <- val.prob(
        p = calibration_dataset_cal$efficient,
        y = calibration_dataset_cal$obs)

      score_cal <- function(r) {

        # # penalizes deviations from (Intercept = 0, Slope = 1) and calibration errors
        # abs(r[["Intercept"]]) +
        #   abs(r[["Slope"]] - 1) +
        #   # r[["Emax"]] +
        #   r[["Eavg"]]
        1 - r[["C (ROC)"]]

      }

      accept <- score_cal(res_2) < score_cal(res)

      if (accept == FALSE) {
        # ----------------------------------------------------------------------
        # calibration doesn't work ---------------------------------------------
        # ----------------------------------------------------------------------
        calibration_model <- NULL
      }

    } else {
      # ------------------------------------------------------------------------
      # NO calibration ---------------------------------------------------------
      # ------------------------------------------------------------------------
      calibration_model <- NULL
    }

    # save results
    save_performance_train[[method_i]] <- performance
    save_best_model_fit[[method_i]] <- model_fit
    save_calibration_model[[method_i]] <- calibration_model

    # WITH VALIDATION SET
    # It is necessary to check the performance of the best model without cv on validation set
    # after that, it is necessary to train again WITH ALL data and the best hyperparameters
    if (!is.null(hold_out)) {

      # --------------------------------------------------------------------
      # performance --------------------------------------------------------
      # --------------------------------------------------------------------

      # check performance on valid data
      if (!is.null(calibration_model)) {

        y_hat <- predict_calibrated(
          newdata = valid_data,
          model_fit = model_fit,
          calibration_model = calibration_model,
          mth_cal = mth_cal
        )

      } else {
        y_hat <- predict(model_fit, newdata = valid_data, type = "prob")[["efficient"]]
      }
      # y_hat <- predict(model_fit, newdata = valid_data, type = "prob")[["efficient"]]
      y_hat_prob <- y_hat
      y_obs <- valid_data$class_efficiency
      y_obs_label <- ifelse(valid_data$class_efficiency == "efficient", 1, 0)

      res <- val.prob(
        p = y_hat_prob,
        y = y_obs_label)

      if (any(is.na(y_hat))) {

        out_names <- c("Accuracy", "Kappa",
                       "Recall", "Specificity", "Precision", "F1", "Balanced_Accuracy", "G_mean",
                       "ROC-AUC", "PR-AUC",
                       "Cross_Entropy", "Cross_Entropy_Efficient")

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

        # save a copy
        same_performance <- performance

        best_imbalance_rate <- as.data.frame(best_balance_i)
        names(best_imbalance_rate) <- "Imbalance_rate"

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

      if(null_calibration) {
        save_calibration_model <- NULL
      }

      output_PEAXAI_models <- list(
        best_model_fit = save_best_model_fit,
        performance_train = save_performance_train,
        performance_train_all = performance_train_all_by_method,
        # calibration_dataset = save_calibrations_dataset,
        calibration_model = save_calibration_model
      )

      return(output_PEAXAI_models)

    }

  }

  # It is necessary to determine the complete facets with ALL data
  get_imbalance <- function(df) as.numeric(sub("\\*$", "", as.character(df["Imbalance_rate"])))

  best_imbalance <- sort(unique(unlist(lapply(save_performance_validation, get_imbalance))), na.last = NA)

  # best_imbalance <- save_performance_validation[[model_i]][, "Imbalance_rate"]

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
          balance_data = best_imbalance_SMOTE,
          seed = seed
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
          balance_data = best_imbalance_SMOTE,
          seed = seed
        )

        datasets_all <- append(train_data_SMOTE, datasets_all)

      }

    }

  }

  # train with ALL data: best balance + hypeparameters
  for (method_i in names(methods)) {

    imbalance_rate_i <- save_performance_validation[[method_i]]$Imbalance_rate

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
      metric_priority = metric_priority,
      seed = seed
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

  if(null_calibration) {
    calibration_model <- NULL
  }

  output_PEAXAI_models <- list(
    best_model_fit = save_best_model_fit,
    performance_validation = save_performance_validation,
    performance_train = save_performance_train,
    performance_train_all = performance_train_all_by_method,
    calibration_model = save_calibration_model
  )

  return(output_PEAXAI_models)

}
