#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param model_fit A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param new_data Parameters for controlling the training process (from the \code{'caret'} package).
#'
#' @importFrom caret train createFolds
#'
#' @return It returns a \code{list} with the chosen model.

performance_ml <- function (
    model_fit, new_data
    ) {

  # predicted data
  y_hat <- predict(
    model_fit, newdata = new_data[, setdiff(names(new_data), "class_efficiency")],
    type = "prob")[,1]

  # save probabilities
  y_hat_prob <- y_hat

  # save levels order; in ML "efficient" first
  levels_order <- c("efficient", "not_efficient")

  # labels -> ML general: 1 efficient level, 2 not_efficient level
  y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")

  # observed data
  y_obs <- new_data$class_efficiency

  # change to factor
  y_hat <- factor(
    y_hat,
    levels = levels_order)

  # calculate confusion matrix
  cm <- confusionMatrix(
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )

  # ROC-AUC
  roc_auc <- pROC::roc(
    response = y_obs,
    predictor = y_hat_prob,
    levels = rev(levels_order),
    direction = "<",
    quiet = TRUE)

  # PR-AUC
  pr_auc <- PRROC::pr.curve(
    scores.class0 = y_hat_prob[y_obs == "not_efficient"],
    scores.class1 = y_hat_prob[y_obs == "efficient"],
    curve = TRUE
  )

  out <- c(
    cm$overall[c("Accuracy", "Kappa")],
    cm$byClass[c("Recall", "Specificity",
                 "Precision", "F1",
                 "Balanced Accuracy")],
    "ROC_AUC" = roc_auc$auc,
    "PR-AUC" = unname(pr_auc$auc.integral)
  )

  out <- as.data.frame(t(out))

  return(out)

}

#' @title Training a Classification Machine Learning Model
#'
#' @description This function trains a set of models and selects best hyperparameters for each of them.
#'
#' @param model_fit A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param new_data Parameters for controlling the training process (from the \code{'caret'} package).
#'
#' @importFrom caret train createFolds
#'
#' @return It returns a \code{list} with the chosen model.

performance_glm <- function (
    model_fit, new_data
) {

  # predicted data
  y_hat <- predict(
    model_fit, newdata = new_data[, setdiff(names(new_data), "class_efficiency")],
    type = "response")

  # save probabilities
  y_hat_prob <- y_hat

  # save levels order; in GLM "not_efficient" first
  levels_order <- c("not_efficient", "efficient")

  # labels -> ML general: 1 efficient level, 2 not_efficient level
  y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")

  # observed data
  new_data$class_efficiency <- factor(
    new_data$class_efficiency,
    levels = levels_order
  )
  y_obs <- new_data$class_efficiency

  # change to factor
  y_hat <- factor(
    y_hat,
    levels = levels_order)

  # calculate confusion matrix
  cm <- confusionMatrix(
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )

  # ROC-AUC
  roc_auc <- pROC::roc(
    response = y_obs,
    predictor = y_hat_prob,
    levels = levels_order,
    direction = "<",
    quiet = TRUE)

  # PR-AUC
  pr_auc <- PRROC::pr.curve(
    scores.class0 = y_hat_prob[y_obs == "not_efficient"],
    scores.class1 = y_hat_prob[y_obs == "efficient"],
    curve = TRUE
  )

  out <- c(
    cm$overall[c("Accuracy", "Kappa")],
    cm$byClass[c("Recall", "Specificity",
                 "Precision", "F1",
                 "Balanced Accuracy")],
    "ROC_AUC" = roc_auc$auc,
    "PR-AUC" = unname(pr_auc$auc.integral)
  )

  out <- as.data.frame(t(out))

  return(out)

}
