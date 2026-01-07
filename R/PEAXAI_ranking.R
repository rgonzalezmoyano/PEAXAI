#' @title Generate Efficiency Rankings Based on Probabilistic Classification
#'
#' @description
#' Produces efficiency rankings of decision-making units (DMUs) according to the probabilities
#' estimated by a fitted classification model. Two ranking modes are supported:
#' \itemize{
#'   \item \code{"predicted"}: ranks DMUs solely by their predicted probability of being efficient.
#'   \item \code{"attainable"}: ranks DMUs hierarchically according to:
#'         (1) the attainable (target) efficiency probability,
#'         (2) the size of the improvement parameter \eqn{\beta} (smaller is better),
#'         and (3) the predicted efficiency probability (higher is better).
#' }
#' This allows to integrate both predictive and counterfactual (attainable) information
#' into the efficiency ranking.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the input and output variables.
#' @param x Integer vector specifying the column indices of input variables in \code{data}.
#' @param y Integer vector specifying the column indices of output variables in \code{data}.
#' @param final_model A fitted classification model used to estimate efficiency probabilities.
#' Supported types are:
#' \itemize{
#'   \item \code{"train"}: an object fitted with \pkg{caret}.
#'   \item \code{"glm"}: a binomial logistic regression model.
#' }
#' @param calibration_model Optional probability-calibration model applied to the raw
#' predicted probabilities from \code{final_model} (e.g., Platt scaling or isotonic regression).
#' If provided, calibrated probabilities are used for ranking and threshold-based decisions.
#' Set to \code{NULL} to use uncalibrated predictions.
#' @param efficiency_thresholds Numeric vector defining one or more efficiency probability
#' thresholds to determine the attainable frontier or peer set.
#' @param targets A named list containing, for each efficiency threshold, the corresponding
#' attainable targets and estimated \eqn{\beta} values (e.g., obtained from counterfactual analysis).
#' Each element should be a list with a component named \code{"beta"}.
#' @param rank_basis Character string specifying the ranking criterion. Options are:
#' \itemize{
#'   \item \code{"predicted"}: order units by predicted efficiency probability.
#'   \item \code{"attainable"}: order by attainable probability, then by \eqn{\beta},
#'   and finally by predicted probability (see Details).
#' }
#'
#' @details
#' The attainable-based ranking combines predictive efficiency with the modeled potential
#' for improvement (\eqn{\beta}) and the probability of reaching a target frontier level.
#' This approach yields a more nuanced and interpretable prioritization of DMUs, reflecting
#' both their current and achievable performance under the estimated model.
#'
#' When \code{rank_basis = "attainable"}, ties in attainable probability are broken first
#' by the magnitude of \eqn{\beta} (ascending), and then by the predicted probability
#' (descending).
#'
#' @return
#' \itemize{
#'   \item If \code{rank_basis = "predicted"}: a \code{data.frame} sorted by predicted efficiency probability.
#'   \item If \code{rank_basis = "attainable"}: a named list of \code{data.frame}s, one per efficiency threshold,
#'   each sorted according to the hierarchical ranking scheme described above.
#' }
#'
#' @importFrom stats predict
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
#'   x <- 1:4
#'   y <- 5
#'   RTS <- "vrs"
#'   imbalance_rate <- NULL
#'
#'   trControl <- list(
#'     method = "cv",
#'     number = 3
#'   )
#'
#'   # glm method
#'   methods <- list(
#'     "glm" = list(
#'       weights = "dinamic"
#'      )
#'   )
#'
#'   metric_priority <- c("Balanced_Accuracy", "ROC_AUC")
#'
#'   models <- PEAXAI_fitting(
#'     data = data, x = x, y = y, RTS = RTS,
#'     imbalance_rate = imbalance_rate,
#'     methods = methods,
#'     trControl = trControl,
#'     metric_priority = metric_priority,
#'     verbose = FALSE,
#'     seed = 1
#'   )
#'
#'   final_model <- models[["best_model_fit"]][["glm"]]
#'
#'   relative_importance <- PEAXAI_global_importance(
#'     data = data, x = x, y = y,
#'     final_model = final_model,
#'     background = "real", target = "real",
#'     importance_method = list(name = "PI", n.repetitions = 5)
#'   )
#'
#'   efficiency_thresholds <- seq(0.75, 0.95, 0.1)
#'
#'   directional_vector <- list(relative_importance = relative_importance,
#'   scope = "global", baseline  = "mean")
#'
#'   targets <- PEAXAI_targets(data = data, x = x, y = y, final_model = final_model,
#'   efficiency_thresholds = efficiency_thresholds, directional_vector = directional_vector,
#'   n_expand = 0.5, n_grid = 50, max_y = 2, min_x = 1)
#'
#'   ranking <- PEAXAI_ranking(data = data, x = x, y = y,
#'   final_model = final_model, rank_basis = "predicted")
#' }
#'
#' @export

PEAXAI_ranking <- function(
    data, x, y, final_model, calibration_model = NULL,
    efficiency_thresholds, targets = NULL, rank_basis
    ) {

  validate_parametes_PEAXAI_ranking(
    data, x, y, final_model,
    efficiency_thresholds, targets, rank_basis
  )

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  # ----------------------------------------------------------------------------
  # predict probability --------------------------------------------------------
  # ----------------------------------------------------------------------------
  data <- as.data.frame(data)

  if (inherits(final_model, "train")) {
    # caret::train
    prob_vector <- PEAXAI_predict(
      data = data,
      x = x,
      y = y,
      final_model = final_model,
      calibration_model = calibration_model
    )
    # prob_vector <- predict(final_model, newdata = data, type = "prob")["efficient"]
  } else {
    stop("Unsupported model type.")
  }

  DMU <- as.data.frame(c(1:nrow(data)))
  names(DMU) <- "DMU"
  prob_vector <- cbind(DMU, prob_vector)
  names(prob_vector)[2] <- "Probability_predicted"

  # ----------------------------------------------------------------------------
  # get ranking ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (rank_basis == "predicted") {

    position <- as.data.frame(1:nrow(prob_vector))
    names(position) <- "Ranking"
    ranking_order <- prob_vector[order(prob_vector[["Probability_predicted"]], decreasing = TRUE), ]

    ranking_order <- cbind(position, ranking_order)
    row.names(ranking_order) <- NULL

    return(ranking_order)

  } else if (rank_basis == "attainable") {

    list_ranking <- vector("list", length = length(efficiency_thresholds))
    names(list_ranking) <- efficiency_thresholds

    for(thr in as.character(efficiency_thresholds)) {

      position <- as.data.frame(1:nrow(prob_vector))
      names(position) <- "Ranking"

      ranking_order <- cbind(prob_vector, targets[[thr]][["inefficiencies"]])
      names(ranking_order)[4] <- "Probability_target"

      ranking_order <- ranking_order[order(
        -ranking_order$Probability_target,
        ranking_order$betas,
        -ranking_order$Probability_predicted),]

      ranking_order <- cbind(position, ranking_order)
      row.names(ranking_order) <- NULL

      list_ranking[[thr]] <- ranking_order

    }

    return(list_ranking)

  }

}
