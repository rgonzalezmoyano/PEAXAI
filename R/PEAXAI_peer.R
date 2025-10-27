#' @title Identify Benchmark Peers Based on Estimated Efficiency Probabilities
#'
#' @description
#' Identifies peer units (i.e., reference benchmarks) for each decision-making unit (DMU) based on predicted probabilities of technical efficiency.
#' Given a fitted classification model that estimates the probability of being efficient, the function selects, for each DMU, its nearest efficient peer
#' according to Euclidean or weighted distances. Multiple efficiency thresholds can be specified to assess different levels of benchmarking stringency.
#'
#' @param data A \code{data.frame} or \code{matrix} containing input and output variables used in the efficiency model.
#' @param x Integer vector indicating the column indices of input variables in \code{data}.
#' @param y Integer vector indicating the column indices of output variables in \code{data}.
#' @param final_model A fitted classification model used to estimate efficiency probabilities. Supported classes: \code{"train"} (from \pkg{caret}) or \code{"glm"} (binomial).
#' @param efficiency_thresholds Numeric vector indicating the minimum probability values required to consider a DMU as efficient.
#' @param weighted Logical. If \code{TRUE}, peers are selected using weighted Euclidean distances based on variable importance. If \code{FALSE} (default), unweighted distances are used.
#' @param relative_importance Optional named numeric vector indicating the relative importance of each input/output variable (used when \code{weighted = TRUE}).
#'
#' @return A named list of matrices. Each element corresponds to an efficiency threshold and contains, for each DMU, the index of the closest efficient peer.
#' If \code{weighted = FALSE}, the list contains unweighted peers. If \code{weighted = TRUE}, the list contains weighted peers.
#'
#' @details
#' This function enables probabilistic peer identification under uncertainty, supporting flexible definitions of efficiency based on thresholds over estimated probabilities.
#' When \code{weighted = TRUE}, variable weights (e.g., derived from feature importance) modulate the peer selection process, allowing for context-aware benchmarking.
#'
#' @importFrom stats predict
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Example using caret model:
#' model <- caret::train(x = ..., y = ..., method = "rf", ...)
#' peers <- PEAXAI_peer(data = mydata, x = 1:4, y = 5, final_model = model,
#'                      efficiency_thresholds = c(0.8, 0.9), weighted = FALSE)
#' }
#'
#' @export

PEAXAI_peer <- function(
    data, x, y, final_model, efficiency_thresholds,
    weighted = FALSE, relative_importance = NULL
    ) {

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)


  variables <- c(x,y)

  # ----------------------------------------------------------------------------
  # predict probability --------------------------------------------------------
  # ----------------------------------------------------------------------------
  data <- as.data.frame(data)

  if (inherits(final_model, "train")) {
    # caret::train
    prob_vector <- predict(final_model, newdata = data, type = "prob")["efficient"]
  } else if (inherits(final_model, "glm")) {
    # glm binomial
    prob_vector <- as.numeric(predict(final_model, newdata = data, type = "response"))
  } else {
    stop("Unsupported model type.")
  }


  # ----------------------------------------------------------------------------
  # get peers ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # data_scenario_list <- list()
  # metrics_list <- list()
  peer_list <- list()
  peer_weight_list <- list()
  na_count_list <- list()
  n_not_prob_list <- list()

  for (thr in efficiency_thresholds) {
    message(paste0("Efficiency threshold: ", thr))


    # check if there are some DMUs which have the threshold desired
    if (max(prob_vector) < thr) {

      peer_list[[thr]] <- NA
      peer_weight_list[[thr]] <- NA
      next
    }

    # first, determinate efficient units
    if (inherits(final_model, "train")) {
      idx_eff <- which(prob_vector$efficient > thr)
    } else if (inherits(final_model, "glm")) {
      idx_eff <- which(prob_vector > thr)
    }

    if (weighted == FALSE) {

      # ------------------------------------------------------------------------
      # normal peers -----------------------------------------------------------
      # ------------------------------------------------------------------------

      # save distances structure
      save_dist <- matrix(
        data = NA,
        ncol = length(idx_eff),
        nrow = nrow(data)
      )

      # calculate distances
      for (unit_eff in idx_eff) {

        # set reference
        reference <- data[unit_eff, variables]

        distance <- unname(apply(data[, variables], 1, function(x) {
          sqrt(sum((x - reference)^2))
        }
        ))

        # get position in save results
        idx_dis <- which(idx_eff == unit_eff)

        save_dist[,idx_dis] <- as.matrix(distance)
      }

      #
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
      peer_list[[as.character(thr)]] <- peer_restult

    } else {

      # ------------------------------------------------------------------------
      # weighted peers ---------------------------------------------------------
      # ------------------------------------------------------------------------

      # save weighted distances structure
      save_dist_weight <- matrix(
        data = NA,
        ncol = length(idx_eff),
        nrow = nrow(data)
      )

      # calculate weighted distances
      result_importance_matrix <- as.data.frame(matrix(
        data = rep(unlist(relative_importance[variables]), each = nrow(data)),
        nrow = nrow(data),
        ncol = ncol(data[,variables]),
        byrow = FALSE
      ))
      names(result_importance_matrix) <- names(data)[variables]

      w_eval_data <- data[, variables] * relative_importance

      for (unit_eff in idx_eff) {

        # set reference
        reference <- data[unit_eff, variables]

        distance <- unname(apply(data[, variables], 1, function(row) {
          sqrt((sum(relative_importance[variables] * ((row - reference)^2))))
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

      peer_restult_weight[, 1] <- idx_eff[near_idx_eff_weight]

      # save_peer
      peer_weight_list[[as.character(thr)]] <- peer_restult_weight

    }

  } # end loop thresholds

  if (weighted == FALSE) {
    return(peer_list)
  } else {
    return(peer_weight_list)
  }

}
