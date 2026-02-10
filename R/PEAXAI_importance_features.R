#' @title Global feature importance for efficiency classifiers
#'
#' @description
#' Computes **global feature importance** for a fitted classification model that
#' separates Pareto-efficient DMUs, using one of three XAI backends:
#' \itemize{
#'   \item \code{"SA"} — Sensitivity Analysis via \pkg{rminer}.
#'   \item \code{"SHAP"} — Model-agnostic SHAP approximations via \pkg{fastshap}.
#'   \item \code{"PI"} — Permutation Importance via \pkg{iml}.
#' }
#' You can evaluate the model on either the training domain (\code{background = "train"})
#' or the real-world domain (\code{background = "real"}) and compute importance on a
#' chosen \code{target} set (\code{"train"} or \code{"real"}). Importances are
#' returned normalized to sum to 1.
#'
#' @param data A \code{data.frame} (or \code{matrix}) with predictors and outcomes.
#'   The function will internally reorder columns to \code{c(x, y)}.
#' @param x Integer or character vector with the columns used as **inputs** (predictors).
#' @param y Integer or character vector with the columns used as **outputs** (targets used
#'   to define \code{class_efficiency} in training; not included in \code{X} when explaining).
#' @param final_model A fitted model. If it is a base-\code{glm} binomial, probabilities
#'   are obtained with \code{type = "response"}; otherwise the function expects
#'   \code{predict(type = "prob")} with a column named \code{"efficient"}.
#' @param background Character, \code{"train"} (default) or \code{"real"}.
#'   Background data define the distribution used for the reference model behaviour.
#' @param target Character, \code{"train"} (default) or \code{"real"}.
#'   Dataset on which importance is computed.
#' @param importance_method A named list (or data.frame-like) with the backend and its args:
#'   \describe{
#'     \item{\code{name}}{One of \code{"SA"}, \code{"SHAP"}, \code{"PI"}.}
#'     \item{\code{method}}{(SA) One of \code{"1D-SA"}, \code{"sens"}, \code{"DSA"}, \code{"MSA"}, \code{"CSA"}, \code{"GSA"}.}
#'     \item{\code{measures}}{(SA) e.g. \code{"AAD"}, \code{"gradient"}, \code{"variance"}, \code{"range"}.}
#'     \item{\code{levels}}{(SA) Discretization levels used by \code{rminer::Importance}.}
#'     \item{\code{baseline}}{(SA) Baseline value for SA, if applicable.}
#'     \item{\code{nsim}}{(SHAP) Number of Monte Carlo samples for \code{fastshap::explain}.}
#'     \item{\code{n.repetitions}}{(PI) Number of permutations per feature for \code{iml::FeatureImp}.}
#'   }
#'
#' @param seed  Integer. Seed for reproducibility.
#'
#' @details
#' Internally, the function builds background/target sets with \code{xai_prepare_sets()}.
#' For \code{glm} models, the positive class is assumed to be the **second level**
#' (\code{"efficient"}) and probabilities are extracted with \code{type = "response"}.
#' For other models (e.g., \pkg{caret}), \code{predict(type = "prob")[, "efficient"]} is used.
#'
#'
#' @return A named numeric vector (or 1-row data.frame) of normalized importances,
#'   with names matching the predictor columns; the values sum to 1.
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
#'    )
#'
#'   metric_priority <- c("Balanced_Accuracy", "ROC_AUC")
#'
#'   models <- PEAXAI_fitting(
#'     data = data, x = x, y = y, RTS = RTS,
#'     imbalance_rate = imbalance_rate,
#'     methods = methods,
#'     trControl = trControl,
#'     metric_priority = metric_priority,
#'     seed = 1,
#'     verbose = FALSE
#'   )
#'
#'   final_model <- models[["best_model_fit"]][["glm"]]
#'
#'   imp <- PEAXAI_global_importance(
#'     data = data, x = x, y = y,
#'     final_model = final_model,
#'     background = "real", target = "real",
#'     importance_method = list(name = "PI", n.repetitions = 5)
#'   )
#'
#'   head(imp)
#' }
#'
#' @seealso \code{\link[fastshap]{explain}}, \code{\link[iml]{FeatureImp}},
#'   \code{\link[rminer]{Importance}}
#'
#' @importFrom stats predict glm
#' @importFrom fastshap explain
#' @importFrom iml FeatureImp Predictor
#' @importFrom rminer Importance
#' @importFrom pROC auc
#'
#' @export

PEAXAI_global_importance <- function(
    data, x, y, final_model, background = "train",
    target = "train", importance_method, seed = 314
    ) {

  validate_parametes_PEAXAI_global_importance(
    data, x, y, final_model,
    background, target, importance_method
  )

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  # reproducibility
  set.seed(seed)

  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------
  # type of prediction
  if (inherits(final_model, "glm")) {
    # type if is a glm model
    type <- "response"
    levels_order <- c("not_efficient", "efficient")
  } else {
    # type if is a ML model
    type <- "prob"
    levels_order <- c("efficient", "not_efficient")
  }

  # prepare datasets, what is going to explain
  xai_prepare_sets <- xai_prepare_sets(
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    background = background,
    target = target,
    type = type,
    threshold  = 0.5,
    levels_order = levels_order
  )

  train_data <- xai_prepare_sets[["train_data"]]
  target_data <- xai_prepare_sets[["target_data"]]

  # methods XAI
  if (importance_method[["name"]] == "SA") {

    # importance with our model of Caret
    f_pred <- function(object, newdata) {

      if (inherits(object, "glm")) {
        predict(object, newdata = newdata, type = type)
      } else {
        predict(object, newdata = newdata, type = type)[, "efficient"]
      }

    }

    # Define methods and measures
    method <- importance_method[["method"]]  # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measure <- importance_method[["measures"]] #  c("AAD", "gradient", "variance", "range")

    levels <- importance_method[["levels"]]

    # matrix of data without label
    if(target == "real") {
      dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    } else {
      dataset_chosen <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
    }

    imp <- rminer::Importance(
      M = final_model,
      data = dataset_chosen,
      RealL = levels,
      method = method,
      measure = measure,
      baseline = importance_method[["baseline"]],
      responses = TRUE,
      PRED = f_pred)[["imp"]]

    imp <- as.data.frame(t(imp))

    names(imp) <- names(dataset_chosen)

    # Normalize
    importance <- imp

  } else if (importance_method[["name"]] == "SHAP") {

    # matrix of data without label
    target_data <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    train_data <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]

    # predict efficiency
    f_pred <- function(object, newdata) {

      if (inherits(object, "glm")) {
        predict(object, newdata = newdata, type = type)
      } else {
        predict(object, newdata = newdata, type = type)[, "efficient"]
      }

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

  } else if (importance_method[["name"]] == "PI") {

    # matrix of data without label
    if(target == "real") {

      labels_DEA <- target_data$class_efficiency
      dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    } else {

      labels_DEA <- train_data$class_efficiency
      dataset_chosen <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
    }

    # predict efficiency
    f_pred <- function(object, newdata) {

      if (inherits(object, "glm")) {
        predict(object, newdata = newdata, type = type)
      } else {
        predict(object, newdata = newdata, type = type)[, "efficient"]
      }

    }

    # 4) Construir el Predictor de iml
    pred_obj <- iml::Predictor$new(
      model = final_model,
      data = dataset_chosen,
      y = labels_DEA,
      predict.function = f_pred,
      type  = "classification"
    )

    # Loss Function by AUC
    if (inherits(final_model, "glm")) {

      loss_auc <- function(truth, estimate) {

        # change to 0/1
        y_bin <- as.numeric(truth == levels(truth)[2])

        if (length(unique(y_bin)) < 2) return(NA_real_)

        1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))

      }

    } else {

      loss_auc <- function(truth, estimate) {

        # change to 0/1
        y_bin <- as.numeric(truth == levels(truth)[1])

        if (length(unique(y_bin)) < 2) return(NA_real_)

        1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))

      }

    }

    # 6) Permutation Importance (repite permutaciones para estabilidad)

    fi <- iml::FeatureImp$new(
      predictor = pred_obj,
      loss = loss_auc,
      compare = "difference",      # caída de rendimiento vs. modelo completo
      n.repetitions = importance_method[["n.repetitions"]] # sube si quieres más estabilidad
    )

    order_names <- names(dataset_chosen)
    idx <- match(order_names, fi$results$feature)

    imp <- fi$results[idx, "importance"]

    imp <- as.data.frame(t(imp))

    names(imp) <- names(dataset_chosen)

    # Normalize
    sum_imp <- sum(imp)
    imp_norm <- imp / sum_imp
    importance <- imp_norm
  }

  result_importance <- importance

  # messages
  if(inherits(final_model, "glm")) {
    name_ML_method <- "glm"
  } else {
    name_ML_method <- final_model[["method"]]
  }

  message(paste0("XAI method: ", importance_method[["name"]], " | Classification model: '", name_ML_method,"'"))
  message(paste("Inputs importance: ", round(sum(result_importance[1:length(x)]), 5)))
  message(paste("Outputs importance: ", round(sum(result_importance[(length(x)+1):(length(c(x, y)))]),5)))

  return(result_importance)
}

#' @title Local feature importance for efficiency classifiers
#'
#' @description
#' Computes **local feature importance** (i.e., per-observation explanations) for a
#' fitted classification model that separates Pareto-efficient DMUs, using one of
#' three XAI backends:
#' \itemize{
#'   \item \code{"SA"} — Local Sensitivity Analysis via \pkg{rminer}.
#'   \item \code{"SHAP"} — Model-agnostic SHAP approximations via \pkg{fastshap}.
#'   \item \code{"LIME"} — Model-agnostic LIME approximations via \pkg{lime}
#' }
#' You can evaluate the model on either the training domain (\code{background = "train"})
#' or the real-world domain (\code{background = "real"}) and compute local importance on a
#' chosen \code{target} set (\code{"train"} or \code{"real"}).
#' For each target DMU, importances are returned normalized to sum to 1 **within that DMU**.
#'
#' @param data A \code{data.frame} (or \code{matrix}) with predictors and outcomes.
#'   The function will internally reorder columns to \code{c(x, y)}.
#' @param x Integer or character vector with the columns used as **inputs** (predictors).
#' @param y Integer or character vector with the columns used as **outputs** (targets used
#'   to define \code{class_efficiency} in training; not included in \code{X} when explaining).
#' @param final_model A fitted model. If it is a base-\code{glm} binomial, probabilities
#'   are obtained with \code{type = "response"}; otherwise the function expects
#'   \code{predict(type = "prob")} with a column named \code{"efficient"}.
#' @param background Character, \code{"train"} (default) or \code{"real"}.
#'   Background data define the reference distribution used when perturbing features.
#' @param target Character, \code{"train"} (default) or \code{"real"}.
#'   Dataset on which **local** importance is computed (one explanation per observation).
#' @param importance_method A named list (or data.frame-like) with the backend and its args:
#'   \describe{
#'     \item{\code{name}}{One of \code{"SA"}, \code{"SHAP"}, \code{"PI"}.}
#'     \item{\code{method}}{(SA) One of \code{"1D-SA"}, \code{"sens"}, \code{"DSA"}, \code{"MSA"}, \code{"CSA"}, \code{"GSA"}.}
#'     \item{\code{measures}}{(SA) e.g. \code{"AAD"}, \code{"gradient"}, \code{"variance"}, \code{"range"}.}
#'     \item{\code{levels}}{(SA) Discretization levels used by \code{rminer::Importance}.}
#'     \item{\code{baseline}}{(SA) Baseline value for SA, if applicable. If omitted, the DMU itself is used as baseline.}
#'     \item{\code{nsim}}{(SHAP) Number of Monte Carlo samples for \code{fastshap::explain}.}
#'     \item{\code{n.repetitions}}{(PI) Number of perturbation/permutation draws per feature and observation.}
#'   }
#'
#' @param seed  Integer. Seed for reproducibility.
#'
#' @details
#' Internally, the function builds background/target sets with \code{xai_prepare_sets()}.
#' Local explanations are produced for the predicted probability \eqn{\hat{p}_i = P(\text{efficient}\mid x_i)}.
#' For \code{glm} models, the positive class is assumed to be the **second level**
#' (\code{"efficient"}) and probabilities are extracted with \code{type = "response"}.
#' For other models (e.g., \pkg{caret}), \code{predict(type = "prob")[, "efficient"]} is used.
#'
#' The meaning of “local importance” depends on the backend:
#' \itemize{
#'   \item \code{"SHAP"} returns per-observation SHAP attributions; importances are computed
#'   from attribution magnitudes (e.g., \code{abs(SHAP)}) and normalized to sum to 1 per DMU.
#'   \item \code{"LIME"} perturbs one feature at a time for a given DMU by sampling from the
#'   background distribution (via permutation/Monte Carlo draws), and measures the mean
#'   absolute change in \eqn{\hat{p}_i}; results are normalized per DMU.
#'   \item \code{"SA"} computes local sensitivity of \eqn{\hat{p}_i} to each feature around
#'   the DMU (using the chosen SA method/measure) and normalizes sensitivities per DMU.
#' }
#'
#' @return A numeric matrix (or data.frame) with one row per target observation and one
#'   column per predictor. Each row is a normalized importance profile whose values sum to 1.
#'   Column names match the predictor columns in \code{x}.
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
#'     )
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
#'     seed = 1,
#'     verbose = FALSE
#'   )
#'
#'   final_model <- models[["best_model_fit"]][["glm"]]
#'
#'   imp_local <- PEAXAI_local_importance(
#'     data = data, x = x, y = y,
#'     final_model = final_model,
#'     background = "real", target = "real",
#'     importance_method = list(name = "SHAP", nsim = 100)
#'   )
#'
#'   head(imp_local)
#' }
#'
#' @seealso \code{\link[fastshap]{explain}},
#'   \code{\link[rminer]{Importance}}
#'
#' @importFrom stats predict glm
#' @importFrom fastshap explain
#' @importFrom rminer Importance
#' @importFrom pROC auc
#'
#' @export

PEAXAI_local_importance <- function(
    data, x, y, final_model, background = "train",
    target = "train", importance_method, seed = 314
) {

  # validate_parametes_PEAXAI_global_importance(
  #   data, x, y, final_model,
  #   background, target, importance_method
  # )

  # reorder index 'x' and 'y' in data
  data <- data[, c(x,y)]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  # reproducibility
  set.seed(seed)

  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------
  # type of prediction
  if (inherits(final_model, "glm")) {
    # type if is a glm model
    type <- "response"
    levels_order <- c("not_efficient", "efficient")
  } else {
    # type if is a ML model
    type <- "prob"
    levels_order <- c("efficient", "not_efficient")
  }

  # prepare datasets, what is going to explain
  xai_prepare_sets <- xai_prepare_sets(
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    background = background,
    target = target,
    type = type,
    threshold  = 0.5,
    levels_order = levels_order
  )

  train_data <- xai_prepare_sets[["train_data"]]
  target_data <- xai_prepare_sets[["target_data"]]

  # methods XAI
  if (importance_method[["name"]] == "SA") {

    # # importance with our model of Caret
    # f_pred <- function(object, newdata) {
    #
    #   if (inherits(object, "glm")) {
    #     predict(object, newdata = newdata, type = type)
    #   } else {
    #     predict(object, newdata = newdata, type = type)[, "efficient"]
    #   }
    #
    # }
    #
    # # Define methods and measures
    # method <- importance_method[["method"]]  # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    # measure <- importance_method[["measures"]] #  c("AAD", "gradient", "variance", "range")
    #
    # levels <- importance_method[["levels"]]
    #
    # # matrix of data without label
    # if(target == "real") {
    #   dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    # } else {
    #   dataset_chosen <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
    # }
    #
    # imp <- rminer::Importance(
    #   M = final_model,
    #   data = dataset_chosen,
    #   RealL = levels,
    #   method = method,
    #   measure = measure,
    #   baseline = importance_method[["baseline"]],
    #   responses = TRUE,
    #   PRED = f_pred)[["imp"]]
    #
    # imp <- as.data.frame(t(imp))
    #
    # names(imp) <- names(dataset_chosen)
    #
    # # Normalize
    # importance <- imp

  } else if (importance_method[["name"]] == "SHAP") {

    # matrix of data without label
    target_data <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    train_data <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]

    # predict efficiency
    f_pred <- function(object, newdata) {

      if (inherits(object, "glm")) {
        predict(object, newdata = newdata, type = type)
      } else {
        predict(object, newdata = newdata, type = type)[, "efficient"]
      }

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
    imp <- data.frame(
      importance = abs(shap_model)
    )
    names(imp) <- names(train_data)

    # Normalize
    row_totals <- rowSums(imp, na.rm = TRUE)
    imp_norm <- imp / row_totals

    importance <- imp_norm

  } else if (importance_method[["name"]] == "LIME") {
browser()
    # # matrix of data without label
    # if(target == "real") {
    #
    #   labels_DEA <- target_data$class_efficiency
    #   dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    # } else {
    #
    #   labels_DEA <- train_data$class_efficiency
    #   dataset_chosen <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
    # }
    #
    # # predict efficiency
    # f_pred <- function(object, newdata) {
    #
    #   if (inherits(object, "glm")) {
    #     predict(object, newdata = newdata, type = type)
    #   } else {
    #     predict(object, newdata = newdata, type = type)[, "efficient"]
    #   }
    #
    # }
    #
    # # 4) Construir el Predictor de iml
    # pred_obj <- iml::Predictor$new(
    #   model = final_model,
    #   data = dataset_chosen,
    #   y = labels_DEA,
    #   predict.function = f_pred,
    #   type  = "classification"
    # )
    #
    # # Loss Function by AUC
    # if (inherits(final_model, "glm")) {
    #
    #   loss_auc <- function(truth, estimate) {
    #
    #     # change to 0/1
    #     y_bin <- as.numeric(truth == levels(truth)[2])
    #
    #     if (length(unique(y_bin)) < 2) return(NA_real_)
    #
    #     1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))
    #
    #   }
    #
    # } else {
    #
    #   loss_auc <- function(truth, estimate) {
    #
    #     # change to 0/1
    #     y_bin <- as.numeric(truth == levels(truth)[1])
    #
    #     if (length(unique(y_bin)) < 2) return(NA_real_)
    #
    #     1 - as.numeric(pROC::auc(response = y_bin, predictor = estimate))
    #
    #   }
    #
    # }
    #
    # # 6) Permutation Importance (repite permutaciones para estabilidad)
    #
    # fi <- iml::FeatureImp$new(
    #   predictor = pred_obj,
    #   loss = loss_auc,
    #   compare = "difference",      # caída de rendimiento vs. modelo completo
    #   n.repetitions = importance_method[["n.repetitions"]] # sube si quieres más estabilidad
    # )
    #
    # order_names <- names(dataset_chosen)
    # idx <- match(order_names, fi$results$feature)
    #
    # imp <- fi$results[idx, "importance"]
    #
    # imp <- as.data.frame(t(imp))
    #
    # names(imp) <- names(dataset_chosen)
    #
    # # Normalize
    # sum_imp <- sum(imp)
    # imp_norm <- imp / sum_imp
    # importance <- imp_norm
  }

  result_importance <- importance

  # messages
  if(inherits(final_model, "glm")) {
    name_ML_method <- "glm"
  } else {
    name_ML_method <- final_model[["method"]]
  }

  message(paste0("XAI method: ", importance_method[["name"]], " | Classification model: '", name_ML_method,"'"))
  message(paste("Inputs importance: ", round(sum(colMeans(result_importance)[1:length(x)]), 5)))
  message(paste("Outputs importance: ", round(sum(colMeans(result_importance)[(length(x)+1):(length(c(x, y)))]),5)))

  return(result_importance)
}
