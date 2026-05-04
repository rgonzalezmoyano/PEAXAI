#' @title Global feature importance for efficiency classifiers
#'
#' @description
#' Computes **global feature importance** for a fitted classification model that
#' separates Pareto-efficient DMUs, using one of three XAI backends:
#' \itemize{
#'   \item \code{"SA"} — Sensitivity Analysis via \pkg{rminer}.
#'   \item \code{"SHAP"} — Model-agnostic SHAP approximations via \pkg{kernelshap}.
#'   \item \code{"PI"} — Permutation Importance via \pkg{iml}.
#' }
#' You can evaluate the model on either the training domain (\code{background = "train"})
#' or the real-world domain (\code{background = "real"}) and compute importance on a
#' chosen \code{target} set (\code{"train"} or \code{"real"}). Importances are
#' returned normalized to sum to 1.
#'
#' @param final_model A fitted model. If it is a base-\code{glm} binomial, probabilities
#'   are obtained with \code{type = "response"}; otherwise the function expects
#'   \code{predict(type = "prob")} with a column named \code{"efficient"}.
#' @param x Integer or character vector with the columns used as **inputs** (predictors).
#' @param y Integer or character vector with the columns used as **outputs** (targets used
#'   to define \code{class_efficiency} in training; not included in \code{X} when explaining).
#' @param explain_data A \code{data.frame} with the original observed DMUs passed
#'   by the user to \code{PEAXAI_fitting()}. This dataset represents the real,
#'   non-augmented observations on which explanations can be computed.
#' @param reference_data A \code{data.frame} with the training dataset used to fit
#'   the optimal model returned by \code{PEAXAI_fitting()}. Depending on the class-
#'   balancing procedure, this dataset may contain only real observed DMUs or both
#'   real and synthetic DMUs.
#' @param importance_method A named list (or data.frame-like) with the backend and its args:
#'   \describe{
#'     \item{\code{name}}{One of \code{"SA"}, \code{"SHAP"}, \code{"PI"}.}
#'     \item{\code{method}}{(SA) One of \code{"1D-SA"}, \code{"sens"}, \code{"DSA"}, \code{"MSA"}, \code{"CSA"}, \code{"GSA"}.}
#'     \item{\code{measures}}{(SA) e.g. \code{"AAD"}, \code{"gradient"}, \code{"variance"}, \code{"range"}.}
#'     \item{\code{levels}}{(SA) Discretization levels used by \code{rminer::Importance}.}
#'     \item{\code{baseline}}{(SA) Baseline value for SA, if applicable.}
#'     \item{\code{bg_n}}{(SHAP) Background sample size for \code{kernelshap::kernelshap} (default: 200).}
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
#'     final_model = final_model, x = x, y = y,
#'     explain_data = data, reference_data = data,
#'     importance_method = list(name = "PI", n.repetitions = 5)
#'   )
#'
#'   head(imp)
#' }
#'
#' @seealso \code{\link[kernelshap]{kernelshap}}, \code{\link[iml]{FeatureImp}},
#'   \code{\link[rminer]{Importance}}
#'
#' @importFrom stats predict glm
#' @importFrom kernelshap kernelshap
#' @importFrom iml FeatureImp Predictor
#' @importFrom rminer Importance
#' @importFrom pROC auc
#'
#' @export

PEAXAI_global_importance <- function(
    final_model, x, y, explain_data, reference_data,
    importance_method, seed = 314
    ) {

  # reproducibility
  set.seed(seed)

  # as data.frame
  explain_data <- as.data.frame(explain_data)
  reference_data <- as.data.frame(reference_data)

  # as data.frame
  explain_data <- as.data.frame(explain_data)
  reference_data <- as.data.frame(reference_data)

  # --------------------------------------------------------

  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------
  # type of prediction
  if (inherits(final_model, "glm")) {
    # type if is a glm model
    type <- "response"
  } else {
    # type if is a ML model
    type <- "prob"
  }

  # format datasets
  format_dataset <- function(d, x_cols, y_cols) {
    if (".outcome" %in% names(d)) {
      names(d)[names(d) == ".outcome"] <- "class_efficiency"
      d <- d[, c(setdiff(names(d), "class_efficiency"), "class_efficiency")]
      return(d)
    } else {
      d <- d[, c(x_cols, y_cols)]
      return(d)
    }
  }

  target_data <- format_dataset(explain_data, x, y)
  train_data <- format_dataset(reference_data, x, y)

  x_len <- length(x)
  y_len <- length(y)

  # --- SECURITY CHECK AGAINST EXTERNAL DATA ---
  n_explain <- nrow(target_data)
  if (n_explain > nrow(train_data)) {
    stop("Error: 'explain_data' has more rows than 'reference_data'. They must be the original DMUs passed to PEAXAI_fitting().")
  }
  # Extract the features from train_data (dropping class_efficiency) to compare
  ref_subset <- train_data[1:n_explain, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
  # Compare formatted datasets
  if (!isTRUE(all.equal(target_data, ref_subset, check.attributes = FALSE))) {
    stop("Error: 'explain_data' features do not match the original training observations. You must provide the same DMUs used in PEAXAI_fitting().")
  }

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
    method <- if (!is.null(importance_method[["method"]])) importance_method[["method"]] else "1D-SA"
    measure <- if (!is.null(importance_method[["measures"]])) importance_method[["measures"]] else "AAD"
    levels <- if (!is.null(importance_method[["levels"]])) importance_method[["levels"]] else 7

    # matrix of data without label
    dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]

    dataset_chosen <- as.data.frame(dataset_chosen)

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

    train_data  <- as.data.frame(train_data)
    target_data <- as.data.frame(target_data)

    bg_n <- if (!is.null(importance_method[["bg_n"]])) importance_method[["bg_n"]] else 200

    #
    shap_model <- kernelshap::kernelshap(
      object = final_model,
      X = target_data,
      bg_X = train_data,
      pred_fun = f_pred,
      bg_n = bg_n
    )

    # global importance = mean |SHAP| per variable
    imp <- t(data.frame(
      importance = colMeans(abs(shap_model$S), na.rm = TRUE)
    ))

    # Normalize for importance relative
    sum_imp <- sum(imp)
    imp_norm <- imp / sum_imp
    importance <- imp_norm

  } else if (importance_method[["name"]] == "PI") {

    labels_DEA <- reference_data$.outcome[1:nrow(target_data)]
    dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]

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
    n_rep <- if (!is.null(importance_method[["n.repetitions"]])) importance_method[["n.repetitions"]] else 5

    fi <- iml::FeatureImp$new(
      predictor = pred_obj,
      loss = loss_auc,
      compare = "difference",      # caída de rendimiento vs. modelo completo
      n.repetitions = n_rep # sube si quieres más estabilidad
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
  message(paste("Inputs importance: ", round(sum(result_importance[1:x_len]), 5)))
  message(paste("Outputs importance: ", round(sum(result_importance[(x_len+1):(x_len + y_len)]),5)))

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
#'   \item \code{"SHAP"} — Model-agnostic SHAP approximations via \pkg{kernelshap}.
#'   \item \code{"LIME"} — Model-agnostic LIME approximations via \pkg{lime}
#' }
#' You can compute local importance on specific DMUs provided in \code{explain_data},
#' using \code{reference_data} as the reference distribution or training background.
#' For each target DMU, importances are returned normalized to sum to 1 **within that DMU**.
#'
#' @param final_model A fitted model. If it is a base-\code{glm} binomial, probabilities
#'   are obtained with \code{type = "response"}; otherwise the function expects
#'   \code{predict(type = "prob")} with a column named \code{"efficient"}.
#' @param x Integer or character vector with the columns used as **inputs** (predictors).
#' @param y Integer or character vector with the columns used as **outputs** (targets used
#'   to define \code{class_efficiency} in training; not included in \code{X} when explaining).
#' @param explain_data A \code{data.frame} with the original observed DMUs (or new DMUs)
#'   on which **local** explanations are computed.
#' @param reference_data A \code{data.frame} with the training dataset used to fit
#'   the optimal model returned by \code{PEAXAI_fitting()}. Depending on the class-
#'   balancing procedure, this dataset may contain only real observed DMUs or both
#'   real and synthetic DMUs. It defines the reference distribution for perturbing features.
#' @param importance_method A named list (or data.frame-like) with the backend and its args:
#'   \describe{
#'     \item{\code{name}}{One of \code{"SA"}, \code{"SHAP"}, \code{"LIME"}.}
#'     \item{\code{method}}{(SA) One of \code{"1D-SA"}, \code{"sens"}, \code{"DSA"}, \code{"MSA"}, \code{"CSA"}, \code{"GSA"}.}
#'     \item{\code{measures}}{(SA) e.g. \code{"AAD"}, \code{"gradient"}, \code{"variance"}, \code{"range"}.}
#'     \item{\code{levels}}{(SA) Discretization levels used by \code{rminer::Importance}.}
#'     \item{\code{baseline}}{(SA) Baseline value for SA, if applicable. If omitted, the DMU itself is used as baseline.}
#'     \item{\code{bg_n}}{(SHAP) Background sample size for \code{kernelshap::kernelshap} (default: 200).}
#'     \item{\code{n_permutations}}{(LIME) Number of permutations per observation (default: 5000).}
#'     \item{\code{feature_select}}{(LIME) Feature selection method, e.g., \code{"auto"}, \code{"none"}, \code{"forward_selection"} (default: \code{"auto"}).}
#'     \item{\code{bin_continuous}}{(LIME) Logical indicating if continuous features should be binned (default: \code{TRUE}).}
#'     \item{\code{n_bins}}{(LIME) Number of bins for continuous features (default: 4).}
#'   }
#'
#' @param seed  Integer. Seed for reproducibility.
#'
#' @details
#' Local explanations are produced for the predicted probability \eqn{\hat{p}_i = P(\text{efficient}\mid x_i)}.
#' For \code{glm} models, the positive class is assumed to be the **second level**
#' (\code{"efficient"}) and probabilities are extracted with \code{type = "response"}.
#' For other models (e.g., \pkg{caret}), \code{predict(type = "prob")[, "efficient"]} is used.
#'
#' The meaning of “local importance” depends on the backend:
#' \itemize{
#'   \item \code{"SHAP"} returns per-observation SHAP attributions; importances are computed
#'   from attribution magnitudes (e.g., \code{abs(SHAP)}) and normalized to sum to 1 per DMU.
#'   \item \code{"LIME"} fits a local surrogate linear model around the DMU using
#'   permutations from the reference distribution; the absolute weights of the local
#'   model are used as importance scores, normalized per DMU.
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
#'     final_model = final_model, x = x, y = y,
#'     explain_data = data, reference_data = data,
#'     importance_method = list(name = "SHAP", bg_n = 200)
#'   )
#'
#'   head(imp_local)
#' }
#'
#' @seealso \code{\link[kernelshap]{kernelshap}},
#'   \code{\link[rminer]{Importance}},
#'   \code{\link[lime]{lime}}
#'
#' @importFrom stats predict glm
#' @importFrom kernelshap kernelshap
#' @importFrom rminer Importance
#' @importFrom pROC auc
#' @importFrom lime lime
#'
#' @export

PEAXAI_local_importance <- function(
    final_model, x, y, explain_data, reference_data,
    importance_method, seed = 314
) {

  # reproducibility
  set.seed(seed)

  # as data.frame
  explain_data <- as.data.frame(explain_data)
  reference_data <- as.data.frame(reference_data)

  # ----------------------------------------------------------------------------
  # detecting importance variables ---------------------------------------------
  # ----------------------------------------------------------------------------
  # type of prediction
  if (inherits(final_model, "glm")) {
    # type if is a glm model
    type <- "response"
  } else {
    # type if is a ML model
    type <- "prob"
  }

  # format datasets
  format_dataset <- function(d, x_cols, y_cols) {
    if (".outcome" %in% names(d)) {
      names(d)[names(d) == ".outcome"] <- "class_efficiency"
      d <- d[, c(setdiff(names(d), "class_efficiency"), "class_efficiency")]
      return(d)
    } else {
      d <- d[, c(x_cols, y_cols)]
      return(d)
    }
  }

  target_data <- format_dataset(explain_data, x, y)
  train_data <- format_dataset(reference_data, x, y)

  x_len <- length(x)
  y_len <- length(y)

  # methods XAI
  if (importance_method[["name"]] == "SA") {

    # falta comprobar
    # importance with our model of Caret
    f_pred <- function(object, newdata) {

      if (inherits(object, "glm")) {
        predict(object, newdata = newdata, type = type)
      } else {
        predict(object, newdata = newdata, type = type)[, "efficient"]
      }

    }

    # Define methods and measures
    method <- if (!is.null(importance_method[["method"]])) importance_method[["method"]] else "1D-SA"
    measure <- if (!is.null(importance_method[["measures"]])) importance_method[["measures"]] else "variance"
    levels <- if (!is.null(importance_method[["levels"]])) importance_method[["levels"]] else 5

    # matrix of data without label
    dataset_chosen <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]

    imp_list <- lapply(seq_len(nrow(dataset_chosen)), function(i) {
      b_i <- dataset_chosen[i, , drop = FALSE]

      rminer::Importance(
        M = final_model,
        data = dataset_chosen,
        RealL = levels,
        method = method,
        measure = measure,
        baseline = b_i,
        responses = TRUE,
        PRED = f_pred
      )[["imp"]]
    })

    imp_mat <- do.call(rbind, imp_list)

    imp <- as.data.frame(imp_mat)

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

    bg_n <- if (!is.null(importance_method[["bg_n"]])) importance_method[["bg_n"]] else 200

    #
    shap_model <- kernelshap::kernelshap(
      object = final_model,
      X = target_data,
      bg_X = train_data,
      pred_fun = f_pred,
      bg_n = bg_n
    )

    # local importance = SHAP per variable
    imp <- data.frame(
      importance = shap_model$S
    )
    names(imp) <- names(train_data)

    # Normalize for relative importance per observation
    imp_abs <- abs(imp)
    row_totals <- rowSums(imp_abs, na.rm = TRUE)
    row_totals[row_totals == 0] <- 1 # prevent division by zero

    importance_abs <- imp_abs / row_totals

    # Reimpose original sign
    importance <- importance_abs * sign(imp)

  } else if (importance_method[["name"]] == "LIME") {

    # matrix of data without label
    target_data <- target_data[, setdiff(names(target_data), "class_efficiency"), drop = FALSE]
    train_data <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]

    train_data  <- as.data.frame(train_data)
    target_data <- as.data.frame(target_data)

    # Determine the correct label name expected by lime
    if (inherits(final_model, "glm")) {
      # lime's default method for glm binomial often names the target "1"
      explain_label <- "1"
    } else {
      explain_label <- "efficient"
    }

    # LIME parameters from importance_method (with defaults)
    bin_continuous <- if (!is.null(importance_method[["bin_continuous"]])) importance_method[["bin_continuous"]] else TRUE
    n_bins         <- if (!is.null(importance_method[["n_bins"]]))         importance_method[["n_bins"]]         else 4
    n_permutations <- if (!is.null(importance_method[["n_permutations"]])) importance_method[["n_permutations"]] else 5000
    feature_select <- if (!is.null(importance_method[["feature_select"]])) importance_method[["feature_select"]] else "auto"

    # create explainer
    explainer <- lime::lime(
      x = train_data,
      model = final_model,
      bin_continuous = bin_continuous,
      n_bins = n_bins
    )

    # run LIME explanation
    lime_model <- lime::explain(
      x = target_data,
      explainer = explainer,
      n_features = ncol(target_data),
      labels = explain_label,
      n_permutations = n_permutations,
      feature_select = feature_select
    )

    # Convert long format to wide matrix (rows = cases, cols = features)
    imp_matrix <- tapply(
      # abs(lime_model$feature_weight),
      lime_model$feature_weight,
      list(as.character(lime_model$case), as.character(lime_model$feature)),
      FUN = sum, na.rm = TRUE
    )

    # Ensure cases and features are ordered correctly to match target_data
    cases_order <- as.character(rownames(target_data))
    if (is.null(cases_order)) cases_order <- as.character(1:nrow(target_data))

    imp <- as.data.frame(imp_matrix[cases_order, names(target_data), drop = FALSE])

    # Replace NAs with 0 (for features not selected by LIME, though n_features = ncol)
    imp[is.na(imp)] <- 0

    # Normalize for relative importance per observation
    imp_abs <- abs(imp)
    row_totals <- rowSums(imp_abs, na.rm = TRUE)
    row_totals[row_totals == 0] <- 1 # prevent division by zero

    importance_abs <- imp_abs / row_totals

    # Reimpose original sign
    importance <- importance_abs * sign(imp)

  }

  result_importance <- importance

  # # messages
  # if(inherits(final_model, "glm")) {
  #   name_ML_method <- "glm"
  # } else {
  #   name_ML_method <- final_model[["method"]]
  # }

  # message(paste0("XAI method: ", importance_method[["name"]], " | Classification model: '", name_ML_method,"'"))
  # message(paste("Inputs importance: ", round(sum(colMeans(result_importance)[1:x_len]), 5)))
  # message(paste("Outputs importance: ", round(sum(colMeans(result_importance)[(x_len+1):(x_len + y_len)]),5)))

  return(result_importance)
}
