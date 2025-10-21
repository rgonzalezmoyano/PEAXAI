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
#' @param final_model A model fitted to use.
#' @param background The data samples used to define the model’s background distribution. Use the training set ("train") to reflect the distribution the model learned from, or the real-world data ("real") to assess model robustness under the actual data domain.
#' @param target The data to be explained/evaluated by the model. Selecting "train" allows understanding how the model makes decisions, while selecting "real" focuses on analyzing the underlying nature of the problem.
#' @param importance_method A \code{data.frame} with the technique and parameters.
#'
#' @importFrom fastshap explain
#'
#'
#' @return A \code{"cafee"} object.
#'
#' @export

PEAXAI_global_importance <- function(
    data, x, y, final_model, background = "train",
    target = "train", importance_method
    ) {

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)


  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------
  # type of prediction
  if (class(final_model)[1] == "train") {
    # type if is a ML model
    type <- "prob"
  } else {
    browser()
    # type if is a glm model
    type <- "response"
  }

  # domain set; by default is "train"
  if (background == "train") {

    # a ML model train by caret
    if (class(final_model)[1] == "train") {

      # save train data, change name and position
      train_data <- final_model[["trainingData"]]
      names(train_data)[1] <- "class_efficiency"

      train_data <- train_data[,c(2:length(train_data),1)]

    } else {

      browser()
    }

  } else if (background == "real") {

    # The training set is the dataset passed via data
    train_data <- data[,c(x,y)]
    train_data$class_efficiency <- predict(
      object = final_model,
      newdata = train_data,
      type = type)[,1]

    train_data$class_efficiency <- ifelse(train_data$class_efficiency > 0.5, "efficient", "not_efficient")

  }

  # target data set; by default is "train"
  if (target == "train") {

    # a ML model train by caret
    if (class(final_model)[1] == "train") {

      # save train data, change name and position
      target_data <- final_model[["trainingData"]]
      names(target_data)[1] <- "class_efficiency"

      target_data <- target_data[,c(2:length(target_data),1)]

    } else {

      browser()
    }

  } else if (target == "real") {

    # The training set is the dataset passed via data
    target_data <- data[,c(x,y)]
    target_data$class_efficiency <- predict(
      object = final_model,
      newdata = target_data,
      type = type)[,1]

    target_data$class_efficiency <- ifelse(target_data$class_efficiency > 0.5, "efficient", "not_efficient")
  }

  # methods XAI
  if (importance_method[["name"]] == "SA") {
#
#     # importance with our model of Caret
#     mypred <- function(M, data) {
#       return (predict(M, data[-length(data)], type = type))
#     }
#
#     # Define methods and measures
#     methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
#     measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
#
#     levels <- 7

  } else if (importance_method[["name"]] == "SHAP") {

    # the fisrt level is efficient
    train_data$class_efficiency <- factor(
      train_data$class_efficiency,
      levels = c("efficient","not_efficient")
    )

    # the fisrt level is efficient
    target_data$class_efficiency <- factor(
      target_data$class_efficiency,
      levels = c("efficient","not_efficient")
    )

    # matrix of data without label
    train_data <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
    target_data <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]

    # predict efficiency
    f_pred <- function(object, newdata) {
      predict(object, newdata = newdata, type = type)[, "efficient"]
    }

    #
    shap_model <- fastshap::explain(
      object = final_model,
      X = train_data,
      pred_wrapper = f_pred,
      newdata = target_data,
      nsim = importance_method[["nsim"]]
    )

    # global importance = mean |SHAP| per variable
    imp <- t(data.frame(
      importance = colMeans(abs(shap_model), na.rm = TRUE)
    ))

    # Normalize
    sum_imp <- sum(imp)
    imp_norm <- imp / sum_imp
    importance <- imp_norm

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

  message(paste("Inputs importance: ", round(sum(result_importance[1:length(x)]), 5)))
  message(paste("Outputs importance: ", round(sum(result_importance[(length(x)+1):(length(c(x, y)))]),5)))

  return(result_importance)
}
