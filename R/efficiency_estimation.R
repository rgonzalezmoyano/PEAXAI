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
#' @param importance_method A \code{string} specifying the method to determine the relative importance of variables. Default includes \code{"SHAP"}.
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
    metric, importance_method, hold_out, balance_data = 0,
    scenarios = 0.75
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

  # metrics for model evaluation
  MySummary <- function(data, lev = NULL, model = NULL) {

    data$pred <- factor(data$pred, levels = lev)
    data$obs  <- factor(data$obs,  levels = lev)

    cm <- caret::confusionMatrix(
      data = data$pred,
      reference = data$obs,
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

    return(out)
  }

  trControl$summaryFunction <- MySummary

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
message(paste0("Training ", key, " method."))
    m <- methods[[key]]

    # save best model if the new if better
    best_model    <- NULL
    best_record   <- NULL

    performance_train <- NULL

    for (dataset in names(datasets_to_train)) {
      message(paste0("Testing with ", dataset, " imbalance rate"))

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
      imb_rate_df <- data.frame(
        imbalance_rate = dataset
      )
      best_row <- cbind(imb_rate_df, best_row)

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

  # ----------------------------------------------------------------------------
  # Train the final model ------------------------------------------------------
  # ----------------------------------------------------------------------------

  # sort by metric
  save_performance <- save_performance %>%
    dplyr::arrange(dplyr::desc(.data[[metric]]))

  # name best method
  final_method <- as.character(save_performance$data[1])

  # names hyparameters
  final_hyparams <- best_models[[final_method]][["bestTune"]]
  final_hyparams <- as.data.frame(final_hyparams)  # asegurar data.frame

  # best imbalance rate
  final_imbalance_rate <- save_performance[1, "imbalance_rate"]

  # final dataset
  if (is.null(hold_out)) {

    # use the balanced dataset
    full_data_for_training <- train_data_SMOTE[[final_imbalance_rate]]

  } else {

    # use train + validation set, need to balanced
    full_data_for_training <- SMOTE_data(
      data = all_data,
      x = x,
      y = y,
      RTS = RTS,
      balance_data = as.numeric(final_imbalance_rate)
    )[[final_imbalance_rate]]

  }

  # 6) recuperar la especificación del modelo ganador (cadena)
  m <- methods[[final_method]]
  final_method_string <- if (!is.null(m$model)) {
    m$model
  } else {
    m
  }

  # 7) entrenar el modelo final con method="none" y tuneGrid=final_hyparams
  #    IMPORTANTE: si usas tu resumen custom, vuelve a fijarlo aquí.
  # MySummary <- trControl$summaryFunction  # si lo definiste arriba
  final_ctrl <- caret::trainControl(method = "none", classProbs = TRUE, summaryFunction = MySummary)

  set.seed(seed)  # asumiendo que 'seed' existe en tu entorno
  ml_model_final <- caret::train(
    form = class_efficiency ~ .,
    data = full_data_for_training,
    method = final_method_string,
    metric = metric,
    trControl = final_ctrl,
    tuneGrid = final_hyparams
  )

  # save the best
  best_models[["__FINAL__"]] <- ml_model_final

  final_model <- ml_model_final

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
