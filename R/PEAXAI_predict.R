#' @title Predict Probability of Efficiency Using a Fitted Model
#'
#' @description
#' Predicts probabilities for new decision-making units (DMUs) using a fitted
#' \pkg{caret} classification model. If \code{calibration_model} is provided,
#' the raw classifier probabilities are post-processed to obtain calibrated
#' probability estimates (e.g., Platt scaling via logistic regression or
#' isotonic calibration via a monotone mapping).
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables used
#'   for prediction.
#' @param x Integer vector indicating the column indices of input variables in
#'   \code{data}.
#' @param y Integer vector indicating the column indices of output variables in
#'   \code{data}.
#' @param final_model A fitted \pkg{caret} model provided by PEAXAI_fitting().
#' @param calibration_model Optional calibration object returned by \code{PEAXAI_fitting()}.
#'   If \code{NULL}, raw probabilities are returned. If \code{calibration_model$method == "glm.fit"},
#'   Platt scaling is applied by predicting \code{type = "response"} from a logistic regression
#'   using the raw score \code{s}. Otherwise, an isotonic calibration function is retrieved via
#'   \code{calibration_model[[final_model$method]]} and applied to the raw probabilities.
#'
#' @return A numeric vector of predicted probabilities (raw or calibrated), one per
#'   row of \code{data}.
#'
#' @export

PEAXAI_predict <- function (
    data, x, y, final_model, calibration_model = NULL
) {

  data <- as.data.frame(data)

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  names_data <- names(data[,c(x,y)])

  name_model <- final_model$method

  if(is.null(calibration_model)) {

    # --------------------------------------------------------------------------
    # No calibration -----------------------------------------------------------
    # --------------------------------------------------------------------------
    predictions <- predict(final_model, newdata = data, type = "prob")[,1]

  } else {

    # --------------------------------------------------------------------------
    # Calibration --------------------------------------------------------------
    # --------------------------------------------------------------------------

    # Original probabilities
    s_new <- predict(final_model,
                     newdata = data,
                     type = "prob")[,1]

    if (calibration_model$method == "glm.fit") {

      # platt
      predictions <- predict(calibration_model,
                             newdata = data.frame(s = s_new),
                             type = "response")
    } else {

      # isotonic
      predictions <- calibration_model[[name_model]](s_new)

    }

  }

  return(predictions)
}
