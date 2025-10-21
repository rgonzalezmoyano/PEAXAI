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
#' @param directional_vector A vector indicating the directional vector.
#' @param imp_global A \code{data.frame} with the importance of features.
#' @param final_model A model fitted to use.
#' @param efficiency_thresholds The level of be
#'
#' @importFrom fastshap explain
#'
#'
#' @return A \code{"cafee"} object.
#'
#' @export

PEAXAI_target <- function(
    data, x, y, final_model, importance_variables,
    directional_vector, efficiency_thresholds
    ) {

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)


  # ----------------------------------------------------------------------------
  # predict probability --------------------------------------------------------
  # ----------------------------------------------------------------------------
  data <- as.data.frame(data)

  eff_vector <- apply(data_rank, 1, function(row) {

    row_df <- as.data.frame(t(row))

    colnames(row_df) <- names(data_rank)

    pred <- unlist(predict(final_model, row_df, type = "prob")[1])

    return(pred)
  })

  # ----------------------------------------------------------------------------
  # get ranking ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
browser()

  eff_vector <- as.data.frame(eff_vector)

  id <- as.data.frame(c(1:nrow(data)))
  names(id) <- "id"
  eff_vector <- cbind(id, eff_vector)

  ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]

#   # ----------------------------------------------------------------------------
#   # to get probabilities scenarios ---------------------------------------------
#   # ----------------------------------------------------------------------------
#   data_scenario_list <- list()
#   metrics_list <- list()
#   peer_list <- list()
#   peer_weight_list <- list()
#   na_count_list <- list()
#   n_not_prob_list <- list()
#
#   for (e in 1:length(efficiency_thresholds)) {
#     message(paste("efficiency_thresholds: ", efficiency_thresholds[e]))
#
#     data_scenario <- compute_target(
#       data = data[,setdiff(names(train_data), "class_efficiency")],
#       x = x,
#       y = y,
#       final_model = final_model,
#       cut_off = efficiency_thresholds[e],
#       imp_vector = result_importance
#     )
#
#     if(all(is.na(data_scenario$data_scenario))) {
#       print("all na")
#       browser()
#
#       # peer
#       peer_restult <- NA
#
#       # save_peer
#       peer_list[[e]] <- peer_restult
#
#       # main_metrics
#       main_metrics <- NA
#
#       # save main_metrics
#       metrics_list[[e]] <- main_metrics
#
#       print("pause")
#
#     } else {
#
#       if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
#
#         data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA
#
#         na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
#         data_scenario$betas[na_idx,] <- NA
#       }
#
#       data_scenario_list[[e]] <- data_scenario
#
#       # ------------------------------------------------------------------------
#       # determinate peer -------------------------------------------------------
#       # ------------------------------------------------------------------------
#
#       # first, determinate efficient units
#       idx_eff <- which(eff_vector$eff_vector > efficiency_thresholds[e])
#
#       if (!length(idx_eff) == 0) {
#
#         # save distances structure
#         save_dist <- matrix(
#           data = NA,
#           ncol = length(idx_eff),
#           nrow = nrow(data)
#         )
#
#         # save weighted distances structure
#         save_dist_weight <- matrix(
#           data = NA,
#           ncol = length(idx_eff),
#           nrow = nrow(data)
#         )
#
#         # calculate distances
#         for (unit_eff in idx_eff) {
#
#           # set reference
#           reference <- data[unit_eff, c(x,y)]
#
#           distance <- unname(apply(data[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
#
#           # get position in save results
#           idx_dis <- which(idx_eff == unit_eff)
#
#           save_dist[,idx_dis] <- as.matrix(distance)
#         }
#
#         near_idx_eff <- apply(save_dist, 1, function(row) {
#
#           which.min(abs(row))
#
#         })
#
#         peer_restult <- matrix(
#           data = NA,
#           ncol = 1,
#           nrow = nrow(data)
#         )
#
#         peer_restult[, 1] <- idx_eff[near_idx_eff]
#
#         # save_peer
#         peer_list[[e]] <- peer_restult
#
#         # eval_data[1, c(x, y)]
#         # eval_data[3, c(x, y)]
#         #
#         # eval_data[1, c(x, y)] - eval_data[3, c(x, y)]
#         #
#         # ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
#         #
#         # result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)]))^2
#         #
#         # sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2))
#         #
#         # sqrt( sum(result_SA * ((eval_data[1, c(x, y)] - eval_data[3, c(x, y)])^2)))
#
#         # calculate weighted distances
#         result_importance_matrix <- as.data.frame(matrix(
#           data = rep(unlist(result_importance[c(x,y)]), each = nrow(data)),
#           nrow = nrow(data),
#           ncol = ncol(data[,c(x,y)]),
#           byrow = FALSE
#         ))
#         names(result_importance_matrix) <- names(data)[c(x,y)]
#
#         w_eval_data <- data[, c(x, y)] * result_importance_matrix
#
#         for (unit_eff in idx_eff) {
#
#           # set reference
#           reference <- data[unit_eff, c(x,y)]
#
#           distance <- unname(apply(data[, c(x, y)], 1, function(row) {
#              sqrt((sum(result_importance[c(x,y)] * ((row - reference)^2))))
#           }))
#
#           # get position in save results
#           idx_dis <- which(idx_eff == unit_eff)
#           save_dist_weight[,idx_dis] <- as.matrix(distance)
#         }
#
#         near_idx_eff_weight <- apply(save_dist_weight, 1, function(row) {
#
#           which.min(abs(row))
#
#         })
#
#         peer_restult_weight <- matrix(
#           data = NA,
#           ncol = 1,
#           nrow = nrow(save_dist_weight)
#         )
#
#         peer_restult_weight[, 1] <- idx_eff[near_idx_eff]
#
#         # save_peer
#         peer_weight_list[[e]] <- peer_restult_weight
#
#         # # join data plus betas to metrics for scenario
#         # data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
#         #
#         # # number not scenario
#         # n_not_prob <- which(data_metrics$probability < scenarios[e])
#         # n_not_prob_list[[e]] <- n_not_prob
#         #
#         # # count na
#         # na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
#         # count_na <- length(na_row)
#         # na_count_list[[e]] <- count_na
#         #
#         # # metrics: mean, median, sd
#         # main_metrics <- as.data.frame(matrix(
#         #   data = NA,
#         #   ncol = ncol(data_metrics),
#         #   nrow = 3
#         # ))
#         #
#         # # metrics
#         # main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
#         # main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
#         # main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)
#         #
#         # names(main_metrics) <- names(data_metrics)
#         # row.names(main_metrics) <- c("mean", "median", "sd")
#         #
#         # metrics_list[[e]] <- main_metrics
#
#       } else {
#
#         peer_list[[e]] <- NULL
#         na_count_list[[e]] <- nrow(data)
#         metrics_list[[e]] <- NULL
#       }
#
#       # join data plus betas to metrics for scenario
#       data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
#
#       # number not scenario
#       n_not_prob <- which(data_metrics$probability < efficiency_thresholds[e])
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
#     }
#
#   } # end loop scenarios
# browser()
#   final_model <- list(
#     final_method = final_method,
#     final_imbalance_rate = final_imbalance_rate,
#     performance_train_all = performance_train_all,
#     save_performance = save_performance,
#     final_model = final_model,
#
#     # # train_decision_balance = train_decision_balance,
#     # real_decision_balance = selected_real_balance,
#
#
#     # performance_train_dataset = selected_model,
#     # performance_real_data = performance_real_data,
#     # importance = importance,
#     result_importance = result_importance,
#     eff_vector = eff_vector,
#     ranking_order = ranking_order,
#     peer_list = peer_list,
#     peer_weight_list = peer_weight_list,
#     data_scenario_list = data_scenario_list,
#     metrics_list = metrics_list,
#     count_na = na_count_list,
#     n_not_prob_list = n_not_prob_list
#   )
#
#   return(final_model)

}
