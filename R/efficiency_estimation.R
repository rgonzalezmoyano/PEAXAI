#' @title Efficiency Estimation Using Classification Algorithms
#'
#' @description This function uses classification algorithms to estimate the efficiency of a set of DMUs (Decision Making Units).
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
#' @param target_method Methodology for labeling the data.
#' @param trControl Parameters for controlling the training process (from the \code{'caret'} package).
#' @param methods A \code{list} of selected machine learning models and their hyperparameters.
#' @param metric A \code{string} specifying the summary metric for classification to select the optimal model. Default includes \code{"Balanced_accuracy"} due to (normally) unbalanced data.
#' @param hold_out A \code{number} value (5-20) for validation data percentage during training (default: 0.2).
#' @param convexity A \code{logical} value indicating if a convex technology is assumed.
#' @param returns Type of returns to scale.
#' @param scenarios Level of efficiency to peer.
#'
#' @importFrom caret trainControl train createDataPartition defaultSummary prSummary
#' @importFrom dplyr select_if %>% arrange top_n sample_n group_split
#' @importFrom Benchmarking dea.boot
#' @importFrom fastDummies dummy_cols
#'
#' @return A \code{"cafee"} object.
#'
#' @export

efficiency_estimation <- function (
    data, x, y, RTS, trControl, methods,
    metric, hold_out, balance_data = 0,
    scenarios = 0.75, convexity = TRUE, returns = "variable"
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

    # --------------------------------------------------------------------------
    # Step 3.1: Addressing imbalance rate --------------------------------------
    # --------------------------------------------------------------------------

  # determine comoplete facets
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

    # --------------------------------------------------------------------------
    # Step 3.2: Select hyperparameters -----------------------------------------
    # --------------------------------------------------------------------------

  # save model and performance
  best_models <- vector("list", length(methods))
  names(best_models) <- names(methods)

  best_performance  <- vector("list", length(methods))
  names(best_performance) <- names(methods)

  # extra: more information about fitting
  performance_train_all <- vector("list", length(methods))
  names(performance_train_all) <- names(methods)

  for (key in names(methods)) {

    m <- methods[[key]]

    # save best model if the new if better
    best_model    <- NULL
    best_record   <- NULL

    performance_train <- NULL

    for (dataset in names(datasets_to_train)) {

      # entrenar
      args <- list(
        form = class_efficiency ~ .,
        data = datasets_to_train[[dataset]],
        method = if (!is.null(m$model)) {
          m$model
          } else {m},
        metric    = metric,
        trControl = trControl
      )

      set.seed(seed)
      ml_model <- do.call(caret::train, args)

      # results of CV
      res <- ml_model$results

      # best tune
      bt  <- ml_model$bestTune

      # names parameters
      pns <- names(bt)

      # index
      idx <- Reduce(`&`, lapply(pns, function(p) res[[p]] == bt[[p]]))
      best_row <- res[idx, , drop = FALSE]

      performance_train <- rbind(performance_train, best_row)

      if (is.null(best_model)) {
        best_record <- best_row[[metric]]
        best_model <- ml_model
        performance <- dataset
      } else if (best_row[[metric]] > best_record) {

        best_record <- best_row[[metric]]
        best_model <- ml_model
        performance <- dataset

      }

    } # datasets

    # guarda SOLO el mejor de esta técnica
    best_models[[key]] <- best_model
    best_performance[[key]] <- performance
    performance_train_all[[key]] <- performance_train

  } # end loop methods

  # ----------------------------------------------------------------------------
  # Validation dataset check ---------------------------------------------------
  # ----------------------------------------------------------------------------

  save_performance <- NULL

  for (key in names(best_models)) {

    # determine y_obs
    y_obs <- valid_data$class_efficiency

    # y_hat (predicted)
    y_hat <- predict(best_models[[key]], newdata = valid_data, type = "prob")[,1]

    # copy
    copy_y_hat <- y_hat

    # labels
    y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
    y_hat <- factor(
      y_hat,
      levels = c("efficient", "not_efficient")
    )

    # check validation performance
    conf_matrix <- caret::confusionMatrix(
      data = y_hat,
      reference = y_obs,
      positive = "efficient"
    )


    # change values to numeric
    y_hat <- copy_y_hat
    y_obs <- ifelse(y_obs == "not_efficient", 0, 1)

    # ROC-AUC
    roc <- pROC::roc(
      response = y_obs,
      predictor = y_hat,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE)

    # PR-AUC
    pr <- PRROC::pr.curve(
      scores.class0 = y_hat[y_obs == 1],
      scores.class1 = y_hat[y_obs == 0],
      curve = TRUE)

    performance <- c(
      conf_matrix$overall[c("Accuracy", "Kappa")],
      conf_matrix$byClass[c("Recall", "Specificity", "Precision", "F1", "Balanced Accuracy")],
    "ROC_AUC" = roc$auc,
    "PR-AUC" = unname(pr$auc.integral))

    performance <- t(as.data.frame(performance))

    df_key <- data.frame(
      data = key
    )

    df_imbalance_rate <- data.frame(
      imbalance_rate = best_performance[[key]]
    )
    performance <- cbind(df_key, df_imbalance_rate, performance)
    rownames(performance) <- NULL

    save_performance <- rbind(save_performance, performance)

  }

  # select the best model by metric
  save_performance <- save_performance %>%
    arrange(desc(metric))

  # ----------------------------------------------------------------------------
  # Train the final model ------------------------------------------------------
  # ----------------------------------------------------------------------------

  final_method <- save_performance[1,1]
  final_hyparams <- best_models[[final_method]][["bestTune"]]
  final_imbalance_rate <- save_performance[1, "imbalance_rate"]

  m <- methods[[final_method]]
  m$model$grid <- final_hyparams
  browser()
  # entrenar
  args <- list(
    form = class_efficiency ~ .,
    data = datasets_to_train[[final_imbalance_rate]],
    method = m$model,
    metric = metric,
    trControl = trainControl(method = "none", classProbs = TRUE)
  )

  set.seed(seed)
  ml_model <- do.call(caret::train, args)



  # # ============================== #
  # # detecting importance variables #
  # # ============================== #
  #
  # # necesary data to calculate importance in rminer
  # train_data <- final_model[["trainingData"]]
  # names(train_data)[1] <- "ClassEfficiency"
  #
  # if(!(is.null(z))) {
  #
  #   dataset_dummy <- dummy_cols(train_data,  select_columns = c(names(train_data))[z+1]) %>%
  #     select(-c(names(train_data))[z+1])
  #
  #   train_data <- dataset_dummy
  #
  #
  #   to_factor <- c((x+y+1):ncol(train_data))
  #   train_data <- change_class(train_data, to_factor = to_factor)
  #
  # } else {
  #
  #   train_data <- train_data[,c(2:length(train_data),1)]
  #
  # }
  #
  # # importance with our model of Caret
  # mypred <- function(M, data) {
  #   return (predict(M, data[-length(data)], type = "prob"))
  # }
  #
  # # Define methods and measures
  # methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  # measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
  #
  # levels <- 7
  #
  # if (names(methods)[i] == "nnet") {
  #   # with rminer
  #   m <- rminer::fit(
  #     ClassEfficiency ~.,
  #     data = train_data,
  #     model = "mlp",
  #     scale = "none",
  #     size = final_model$bestTune$size,
  #     decay = final_model$bestTune$decay
  #     #entropy = FALSE
  #     #softmax = TRUE
  #   )
  #
  #   # Calculate the importance for the current method and measure
  #   importance <- Importance(
  #     M = m,
  #     RealL = levels, # Levels
  #     data = train_data,
  #     method = methods_SA,
  #     measure = measures_SA,
  #     baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
  #     responses = TRUE
  #
  #   )
  #
  # } else {
  #   # Calculate the importance for the current method and measure
  #   importance <- Importance(
  #     M = final_model$final_model$finalModel,
  #     RealL = levels, # Levels
  #     data = train_data, # data
  #     method = methods_SA,
  #     measure = measures_SA,
  #     baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
  #     responses = TRUE,
  #     PRED = mypred,
  #     outindex = length(train_data) # length(train_data)
  #   )
  # }
  #
  # result_SA <- as.data.frame(t((round(importance$imp, 3))))[, -length(importance$imp)]
  # rownames(result_SA) <- NULL
  # names(result_SA) <- names(train_data)[-length(train_data)]
  #
  # if (names(methods) == "nnet") {
  #   final_model_p <- final_model
  # } else {
  #   final_model_p <- final_model$final_model
  # }
  #
  # print(paste("Inputs importance: ", sum(result_SA[1:length(x)])))
  # print(paste("Outputs importance: ", sum(result_SA[(length(x)+1):(length(x)+length(y))])))
  # #print(paste("Environment importance: ", sum(result_SA[((length(x) + length(y))+1):length(result_SA)])))
  # #print(paste("Seed: ", seed))
  #
  # # =========== #
  # # get ranking #
  # # =========== #
  #
  # if (nZ != 0) {
  #   data_rank <- data_save[, c(x,y,z)]
  #   data_rank <- as.data.frame(data_rank)
  #   } else {
  #   data_rank <- data_save[, c(x,y)]
  #   data_rank <- as.data.frame(data_rank)
  #   }
  #
  # eff_vector <- apply(data_rank, 1, function(row) {
  #
  #   row_df <- as.data.frame(t(row))
  #
  #   colnames(row_df) <- names(data_rank)
  #
  #   if (!(is.null(z))) {row_df <- change_class(data = row_df, to_numeric = c(x,y), to_factor = z)}
  #
  #   pred <- unlist(predict(final_model_p, row_df, type = "prob")[1])
  #
  #   return(pred)
  # })
  #
  # eff_vector <- as.data.frame(eff_vector)
  #
  # id <- as.data.frame(c(1:nrow(data_save)))
  # names(id) <- "id"
  # eff_vector <- cbind(id, eff_vector)
  #
  # ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]
  #
  # # ============================= #
  # # to get probabilities senarios #
  # # ============================= #
  #
  # data_scenario_list <- list()
  # metrics_list <- list()
  # peer_list <- list()
  # peer_weight_list <- list()
  # na_count_list <- list()
  # n_not_prob_list <- list()
  #
  # for (e in 1:length(scenarios)) {
  #   print(paste("scenario: ", scenarios[e]))
  #   print(final_model)
  #
  #   data_scenario <- compute_target (
  #     data = data_save,
  #     x = x,
  #     y = y,
  #     z = z,
  #     final_model = final_model,
  #     cut_off = scenarios[e],
  #     imp_vector = result_SA
  #   )
  #
  #   if(all(is.na(data_scenario$data_scenario))) {
  #     print("all na")
  #     browser()
  #
  #     # peer
  #     peer_restult <- NA
  #
  #     # save_peer
  #     peer_list[[e]] <- peer_restult
  #
  #     # main_metrics
  #     main_metrics <- NA
  #
  #     # save main_metrics
  #     metrics_list[[e]] <- main_metrics
  #
  #     print("pause")
  #
  #   } else {
  #
  #     if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
  #
  #       data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA
  #
  #       na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
  #       data_scenario$betas[na_idx,] <- NA
  #     }
  #
  #     data_scenario_list[[e]] <- data_scenario
  #
  #     # ================ #
  #     # determinate peer #
  #     # ================ #
  #
  #     # first, determinate efficient units
  #     idx_eff <- which(eff_vector$eff_vector > scenarios[e])
  #
  #     if (!length(idx_eff) == 0) {
  #
  #       # save distances structure
  #       save_dist <- matrix(
  #         data = NA,
  #         ncol = length(idx_eff),
  #         nrow = nrow(data_save)
  #       )
  #
  #       # save weighted distances structure
  #       save_dist_weight <- matrix(
  #         data = NA,
  #         ncol = length(idx_eff),
  #         nrow = nrow(data_save)
  #       )
  #
  #       # calculate distances
  #       for (unit_eff in idx_eff) {
  #
  #         # set reference
  #         reference <- data_save[unit_eff, c(x,y)]
  #
  #         distance <- unname(apply(data_save[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
  #
  #         # get position in save results
  #         idx_dis <- which(idx_eff == unit_eff)
  #
  #         save_dist[,idx_dis] <- as.matrix(distance)
  #       }
  #
  #       near_idx_eff <- apply(save_dist, 1, function(row) {
  #
  #         which.min(abs(row))
  #
  #       })
  #
  #       peer_restult <- matrix(
  #         data = NA,
  #         ncol = 1,
  #         nrow = nrow(data_save)
  #       )
  #
  #       peer_restult[, 1] <- idx_eff[near_idx_eff]
  #
  #       # save_peer
  #       peer_list[[e]] <- peer_restult
  #
  #       # eval_data[1, c(x, y)]
  #       # eval_data[3, c(x, y)]
  #       #
  #       # eval_data[1, c(x, y)] - eval_data[3, c(x, y)]
  #       #
  #       # ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
  #       #
  #       # result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
  #       #
  #       # sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2))
  #       #
  #       # sqrt( sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2)))
  #
  #       # calculate weighted distances
  #       result_SA_matrix <- as.data.frame(matrix(
  #         data = rep(unlist(result_SA[c(x,y)]), each = nrow(data_save)),
  #         nrow = nrow(data_save),
  #         ncol = ncol(data_save[,c(x,y)]),
  #         byrow = FALSE
  #       ))
  #       names(result_SA_matrix) <- names(data_save)[c(x,y)]
  #
  #       w_eval_data <- data_save[, c(x, y)] * result_SA_matrix
  #
  #       for (unit_eff in idx_eff) {
  #
  #         # set reference
  #         reference <- data_save[unit_eff, c(x,y)]
  #
  #         distance <- unname(apply(data_save[, c(x, y)], 1, function(row) {
  #            sqrt((sum(result_SA[c(x,y)] * ((row - reference)^2))))
  #         }))
  #
  #         # get position in save results
  #         idx_dis <- which(idx_eff == unit_eff)
  #         save_dist_weight[,idx_dis] <- as.matrix(distance)
  #       }
  #
  #       near_idx_eff_weight <- apply(save_dist_weight, 1, function(row) {
  #
  #         which.min(abs(row))
  #
  #       })
  #
  #       peer_restult_weight <- matrix(
  #         data = NA,
  #         ncol = 1,
  #         nrow = nrow(save_dist_weight)
  #       )
  #
  #       peer_restult_weight[, 1] <- idx_eff[near_idx_eff]
  #
  #       # save_peer
  #       peer_weight_list[[e]] <- peer_restult_weight
  #
  #       # join data plus betas to metrics for scenario
  #       data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
  #
  #       # number not scenario
  #       n_not_prob <- which(data_metrics$probability < scenarios[e])
  #       n_not_prob_list[[e]] <- n_not_prob
  #
  #       # count na
  #       na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
  #       count_na <- length(na_row)
  #       na_count_list[[e]] <- count_na
  #
  #       # metrics: mean, median, sd
  #       main_metrics <- as.data.frame(matrix(
  #         data = NA,
  #         ncol = ncol(data_metrics),
  #         nrow = 3
  #       ))
  #
  #       # metrics
  #       main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
  #       main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
  #       main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)
  #
  #       names(main_metrics) <- names(data_metrics)
  #       row.names(main_metrics) <- c("mean", "median", "sd")
  #
  #       metrics_list[[e]] <- main_metrics
  #
  #     } else {
  #
  #       peer_list[[e]] <- NULL
  #       na_count_list[[e]] <- nrow(eval_data)
  #       metrics_list[[e]] <- NULL
  #     }
  #
  #   }
  #
  # } # end loop scenarios
  #
  # # names(data_scenario_list) <- scenarios
  # #
  # # names(na_count_list) <- scenarios
  # #
  # # if (!(length(metrics_list) == 0)) {
  # #
  # #   if (!(length(metrics_list)) == length(scenarios)) {
  # #     scenarios <- scenarios[1:length(metrics_list)]
  # #   }
  # #
  # #   names(metrics_list) <- scenarios
  # #   names(peer_list) <- scenarios
  # #   names(peer_weight_list) <- scenarios
  # #   names(n_not_prob_list) <- scenarios
  # # }
  # #
  # # # check real data performance
  # # y_obs <- eval_data$class_efficiency
  # # y_hat <- predict(best_ml_model, eval_data)
  # #
  # #create confusion matrix and calculate metrics related to confusion matrix
  # performance_real_data <- confusionMatrix (
  #   data = y_hat,
  #   reference = y_obs,
  #   mode = "everything",
  #   positive = "efficient"
  # )[["byClass"]]
  #
  # final_model <- list(
  #   train_decision_balance = train_decision_balance,
  #   real_decision_balance = selected_real_balance,
  #   best_balance = best_balance,
  #   final_model = final_model,
  #   performance_train_dataset = selected_model,
  #   performance_real_data = performance_real_data,
  #   importance = importance,
  #   result_SA = result_SA,
  #   eff_vector = eff_vector,
  #   ranking_order = ranking_order,
  #   peer_list = peer_list,
  #   peer_weight_list = peer_weight_list,
  #   data_scenario_list = data_scenario_list,
  #   metrics_list = metrics_list,
  #   count_na = na_count_list,
  #   n_not_prob_list = n_not_prob_list
  # )
  #
  # return(final_model)

}

#' @title Select cut-off point in Classification Algorithm
#'
#' @description This function selects the cut-off point that minimizes the sum of false positives and false negatives.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param final_model The best \code{train} object from \code{caret}.
#'
#' @return It returns the best cut-off point.

select_cut_off <- function (
    data, final_model
    ) {

  # cut-off points
  df_cp <- data.frame (
    cut_off_points = seq(0, 1, by = 0.01),
    false_pred = NA
  )

  # predictions
  y_hat <- predict(final_model, data, type = "prob")

  for (i in 1:nrow(df_cp)) {

    # cut-off point
    cp_point <- df_cp[i, "cut_off_points"]

    # predictions for a given cut-off point
    y_hat_cp <- ifelse(y_hat$efficient >= cp_point, "efficient", "not_efficient")
    y_hat_cp <- factor(y_hat_cp, levels = c("efficient", "not_efficient"))

    # confusion matrix
    cm_cp <- confusionMatrix (
      data = y_hat_cp,
      reference = data$class_efficiency,
      mode = "everything",
      positive = "efficient"
    )[["table"]]

    # compute false positive and false negative
    df_cp[i, "false_pred"] <- cm_cp[2, 1] + cm_cp[1, 2]
  }

  # minimum cost for the cut-off point
  min_value <- min(df_cp$false_pred)
  min_index <- which(df_cp$false_pred == min_value)
  cut_point <- df_cp[max(min_index), "cut_off_points"]

  return(cut_point)
}
