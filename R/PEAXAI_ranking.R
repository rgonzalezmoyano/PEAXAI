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
#' \dontrun{
#' # Example with caret model:
#' model <- caret::train(x = ..., y = ..., method = "rf", ...)
#' rankings <- PEAXAI_ranking(
#'   data = mydata,
#'   x = 1:3, y = 4:5,
#'   final_model = model,
#'   efficiency_thresholds = c(0.8, 0.9),
#'   targets = my_targets,
#'   rank_basis = "attainable"
#' )
#' }
#'
#' @export

PEAXAI_ranking <- function(
    data, x, y, final_model, efficiency_thresholds,
    targets, rank_basis
    ) {

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
    prob_vector <- predict(final_model, newdata = data, type = "prob")["efficient"]
  } else if (inherits(final_model, "glm")) {
    # glm binomial
    prob_vector <- as.numeric(predict(final_model, newdata = data, type = "response"))
  } else {
    stop("Unsupported model type.")
  }

  id <- as.data.frame(c(1:nrow(data)))
  names(id) <- "ID"
  prob_vector <- cbind(id, prob_vector)
  names(prob_vector)[2] <- "probability_predicted"

  # ----------------------------------------------------------------------------
  # get ranking ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (rank_basis == "predicted") {

    ranking_order <- prob_vector[order(prob_vector$probability_predicted, decreasing = TRUE), ]

    return(ranking_order)

  } else if (rank_basis == "attainable") {

    list_ranking <- vector("list", length = length(efficiency_thresholds))
    names(list_ranking) <- efficiency_thresholds

    for(thr in as.character(efficiency_thresholds)) {

      ranking_order <- cbind(prob_vector, targets[[thr]][["beta"]])
      names(ranking_order)[4] <- "probability_target"

      ranking_order <- ranking_order[order(
        -ranking_order$probability_target,
        ranking_order$beta,
        -ranking_order$probability_predicted),]

      list_ranking[[thr]] <- ranking_order
    }

    return(list_ranking)

  }

}
