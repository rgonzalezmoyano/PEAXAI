#' @title Validate PEAXAI_fitting input parameters
#'
#' @description
#' Validates arguments for PEAXAI fitting/projection routines. It checks container
#' types, index ranges, non-overlapping input/output indices, admissible returns-to-scale
#' codes, resampling configuration, and basic probability ranges.
#'
#' @param data A \code{data.frame} or \code{matrix} with the variables used in the model.
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param RTS Returns-to-scale specification. Either a numeric code in
#'   \code{c(0,1,2,3,4,5)} or a character in
#'   \code{c("fdh","vrs","drs","crs","irs","add")}, matching \code{Benchmarking::dea.add()}.
#' @param trControl A list with resampling/control settings, e.g.
#'   \code{list(method = "cv", number = 5)}. Currently, \code{method} must be
#'   \code{"cv"} and \code{number} must be numeric.
#' @param methods A \strong{named list} of learners and their options (e.g., tuning grids),
#'   e.g. \code{list("nnet" = list(tuneGrid = ...), "svmPoly" = list(tuneGrid = ...))}.
#'   Allowed learner keys are \code{c("nnet","svmPoly","glm")}.
#' @param metric_priority Character vector with one or more primary metrics
#'   (e.g., \code{"ROC-AUC"}, \code{"Accuracy"}). Must belong to the supported set.
#' @param hold_out Numeric proportion in (0,1) (used if applicable by downstream code).
#' @param imbalance_rate Optional numeric vector with class proportions or target imbalance
#'   rates. All values must lie strictly in \code{(0,1)} if provided.
#' @param verbose Logical; whether to print additional diagnostics (validated as scalar logical).
#'
#' @details
#' Checks performed include:
#' \itemize{
#'   \item \strong{Data container}: \code{data} must be a non-empty \code{data.frame} or \code{matrix}.
#'   \item \strong{Index vectors}: \code{x} and \code{y} are numeric indices within \code{ncol(data)} and must not overlap.
#'   \item \strong{RTS}: must be one of \code{0:5} or \code{"fdh","vrs","drs","crs","irs","add"}.
#'   \item \strong{Resampling}: \code{trControl$method == "cv"} with numeric \code{trControl$number}.
#'   \item \strong{Methods}: names must include at least one of \code{"nnet","svmPoly","glm"}.
#'   \item \strong{Imbalance}: if provided, all entries of \code{imbalance_rate} are in \code{(0,1)}.
#' }
#'
#' @return Invisibly returns \code{NULL} on success; otherwise throws an error describing the first issue found.
#'
#' @seealso \code{\link[Benchmarking]{dea.add}} for RTS codes; \code{\link[caret]{trainControl}} for CV specs.
#'
#' @keywords internal
#' @noRd

validate_parametes_PEAXAI_fitting <- function(
    data, x, y, RTS, trControl, methods, metric_priority,
    hold_out, imbalance_rate, verbose
) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }
  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # RTS ------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  RTS_available_num  <- 0:5
  RTS_available_char <- c("fdh","vrs","drs","crs","irs","add")

  if (is.numeric(RTS) && !RTS %in% RTS_available_num)
    stop("'RTS' not recognized.", call. = FALSE)
  if (is.character(RTS) && !RTS %in% RTS_available_char)
    stop("'RTS' not recognized.", call. = FALSE)

  # ----------------------------------------------------------------------------
  # trControl ------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (is.null(trControl) || (is.list(trControl) && length(trControl) == 0L)) {
    stop("'trControl' must be specified (e.g., list(method = 'cv', number = 5)).", call. = FALSE)
  }

  if (!is.character(trControl[["method"]]) ||
      length(trControl[["method"]]) != 1L ||
      !(trControl[["method"]] %in% c("cv", "repeatedcv"))) {
    stop("`trControl$method` must be 'cv' or 'repeatedcv'.", call. = FALSE)
  }

  if (trControl[["method"]] == "cv") {
    if (is.null(trControl[["number"]]) || !is.numeric(trControl[["number"]])) {
      stop("`trControl$number` must be numeric.", call. = FALSE)
    }
  } else if (trControl[["method"]] == "repeatedcv") {

    if (is.null(trControl[["number"]]) || is.null(trControl[["repeats"]]) || !is.numeric(trControl[["number"]]) || !is.numeric(trControl[["repeats"]])) {
      stop("`trControl$number` and `trControl$repeats` must be numeric.", call. = FALSE)
    }
  }


  # ----------------------------------------------------------------------------
  # Methods --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (is.null(methods) || !is.list(methods)) {
    stop("`methods` must be a named list.", call. = FALSE)
  }
  if (!any(names(methods) %in% c("nnet","svmPoly","glm"))) {
    stop("`methods` must include at least one of: nnet, svmPoly, glm.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Metric priority ------------------------------------------------------------
  # ----------------------------------------------------------------------------
  metric_priority_available <- c("Accuracy", "Kappa",
                                 "Recall", "Specificity",
                                 "Precision", "F1", "Balanced_Accuracy", "G_mean",
                                 "ROC_AUC", "PR_AUC")
  if (!any(metric_priority %in% metric_priority_available)) {
    stop("`metric_priority` must be in the supported set.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Hold out -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.null(hold_out)) {
    if (!is.numeric(hold_out) || hold_out <= 0 || hold_out >= 1) {
      stop("`hold_out` must be numeric in (0,1).", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # Imbalance rate -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.null(imbalance_rate)) {
    if (!is.numeric(imbalance_rate) || any(imbalance_rate <= 0 | imbalance_rate >= 1)) {
      stop("'imbalance_rate' values must be in (0,1).", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # verbose --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("'verbose' must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' @title Validate input parameters for efficiency labeling (Additive DEA)
#'
#' @description
#' Checks that the arguments supplied to the efficiency labeling routine are
#' correctly specified. The function validates container types, index ranges,
#' non-overlapping input/output indices, returns-to-scale codes, and basic
#' dimensional consistency between \code{data} and \code{REF}.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables used
#'   for labeling. Rows correspond to DMUs.
#' @param REF Optional reference set with the same number of rows as \code{data}
#'   used to define the technology. Defaults to \code{data}. Must be a
#'   \code{data.frame} or \code{matrix}.
#' @param x Numeric vector of column indices in \code{data}/\code{REF}
#'   corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data}/\code{REF}
#'   corresponding to output variables.
#' @param RTS Returns-to-scale specification. Either a numeric code in
#'   \code{c(0,1,2,3,4,5)} or a character in
#'   \code{c("fdh","vrs","drs","crs","irs","add")}, matching the options in
#'   \code{Benchmarking::dea.add()}.
#'
#' @details
#' The following checks are performed:
#' \itemize{
#'   \item \strong{Containers}: \code{data} (and \code{REF}) must be non-empty
#'         \code{data.frame} or \code{matrix} objects with the same number of rows.
#'   \item \strong{Index vectors}: \code{x} and \code{y} must be numeric index
#'         vectors within \code{ncol(data)} and must not overlap.
#'   \item \strong{RTS}: must be either one of the numeric codes \code{0:5} or one of
#'         \code{"fdh","vrs","drs","crs","irs","add"} (as in \code{Benchmarking::dea.add}).
#' }
#' Any violation triggers \code{stop()} with a descriptive message.
#'
#' @return
#' Invisibly returns \code{NULL} on success. Otherwise, throws an error describing
#' the first problem encountered.
#'
#' @seealso
#' \code{\link[Benchmarking]{dea.add}} for RTS codes and the Additive DEA model.
#'
#' @keywords internal
#' @noRd

# check if parameters are well introduced
validate_parametes_label_efficiency <- function(data, REF = NULL, x, y, RTS) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # REF ------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # REF: si es NULL, usa data; si no, valida
  if (is.null(REF)) {
    REF <- data
  } else {
    if (!is.data.frame(REF) && !is.matrix(REF)) {
      stop("'REF' must be a data.frame or matrix.", call. = FALSE)
    }
    if (nrow(REF) == 0L || ncol(REF) == 0L) {
      stop("'REF' has zero rows or zero columns.", call. = FALSE)
    }
    if (nrow(REF) != nrow(data)) {
      stop("'REF' must have the same number of rows as 'data'.", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }
  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # RTS ------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  RTS_available_num  <- 0:5
  RTS_available_char <- c("fdh","vrs","drs","crs","irs","add")
  if (is.numeric(RTS) && !RTS %in% RTS_available_num)
    stop("'RTS' not recognized.", call. = FALSE)
  if (is.character(RTS) && !RTS %in% RTS_available_char)
    stop("'RTS' not recognized.", call. = FALSE)

  invisible(NULL)
}

#' @title Validate input parameters for SMOTE preprocessing
#'
#' @description
#' Validates arguments used before applying SMOTE (or class-balancing) in the
#' PEAXAI pipeline. It checks container types, index ranges, non-overlapping
#' input/output indices, admissible returns-to-scale (RTS) codes (for downstream
#' DEA-related steps), and class-balance proportions.
#'
#' @param data A \code{data.frame} or \code{matrix} with the variables used in the model.
#'   Rows correspond to DMUs (observations).
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param RTS Returns-to-scale specification. Either a numeric code in
#'   \code{c(0,1,2,3,4,5)} or a character in
#'   \code{c("fdh","vrs","drs","crs","irs","add")}, matching \code{Benchmarking::dea.add()}.
#'   (Kept for consistency with downstream DEA steps; not specific to SMOTE.)
#' @param balance_data Optional numeric value or vector with target class balance
#'   proportions in the open interval \code{(0,1)}. For a single binary target, a
#'   scalar can represent the desired positive-class proportion (e.g., \code{0.5}).
#'
#' @details
#' Checks performed include:
#' \itemize{
#'   \item \strong{Data container}: \code{data} must be a non-empty \code{data.frame} or \code{matrix}.
#'   \item \strong{Index vectors}: \code{x} and \code{y} are numeric indices within \code{ncol(data)} and must not overlap.
#'   \item \strong{RTS}: must be one of \code{0:5} or \code{"fdh","vrs","drs","crs","irs","add"}.
#'   \item \strong{Balance}: if \code{balance_data} is provided, all entries must lie strictly in \code{(0,1)}.
#' }
#'
#' @return Invisibly returns \code{NULL} on success; otherwise throws an error describing the first issue found.
#'
#' @seealso \code{\link[Benchmarking]{dea.add}} for RTS codes (DEA step).
#'
#' @keywords internal
#' @noRd
#'

# check if parameters are well introduced
validate_parametes_SMOTE_data <- function(
  # SMOTE_data
  data, x, y, RTS, balance_data

) {

  # ----------------------------------------------------------------------------
  # Data --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }
  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # RTS ------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  RTS_available_num  <- 1:5
  RTS_available_char <- c("vrs","drs","crs","irs","add")

  if (is.numeric(RTS) && !RTS %in% RTS_available_num) {
    stop("'RTS' not recognized.", call. = FALSE)
  }

  if (is.character(RTS) && !RTS %in% RTS_available_char) {
    stop("'RTS' not recognized.", call. = FALSE)
  }

  if (any(RTS == "0" || RTS == 0)) {
    stop("'fdh' in RTS' is not possible in SMOTE data", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Imbalance rate -------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.null(balance_data)) {
    if (!is.numeric(balance_data) || any(balance_data <= 0 | balance_data >= 1)) {
      stop("'balance_data' values must be in (0,1).", call. = FALSE)
    }
  }

  invisible(NULL)
}

#' @title Validate input parameters for PEAXAI global importance
#'
#' @description
#' Validates arguments prior to computing global feature importance in the PEAXAI
#' pipeline. It checks data containers, input/output index ranges, returns-to-scale
#' (RTS) codes used in downstream DEA steps, the trained model object, the origin
#' of background/target data, and the global importance method specification
#' using packages: \strong{rminer} (SA), \strong{fastshap} (SHAP), and \strong{iml} (PI).
#'
#' @param data A \code{data.frame} or \code{matrix} with the variables used in the model
#'   (rows = DMUs/observations).
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param final_model A trained \code{caret} model, i.e., an object of class \code{"train"}.
#' @param background Background data origin for global explanations: \code{"train"} or \code{"real"}.
#' @param target Target data origin to be explained: \code{"train"} or \code{"real"}.
#' @param importance_method A \code{list} specifying the global importance approach.
#'   Must include \code{name} in \code{c("SA","SHAP","PI")}. Accepted fields by method:
#'   \itemize{
#'     \item \strong{SA} (via \pkg{rminer}): \code{method} in
#'           \code{c("1D-SA","sens","DSA","MSA","CSA")};
#'           \code{measures} in \code{c("AAD","gradient","variance","range")};
#'           \code{levels} single integer >= 2;
#'           \code{baseline} either \code{"mean"}, \code{"media"} or a \code{data.frame}.
#'     \item \strong{SHAP} (via \pkg{fastshap}): \code{nsim} single positive integer.
#'     \item \strong{PI} (via \pkg{iml}): \code{n.repetitions} single positive integer.
#'   }
#'
#' @details
#' The function throws an informative error on the first detected violation. On success,
#' it returns \code{NULL} invisibly.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @seealso \code{\link[caret]{train}} for the \code{"train"} object;
#'   \code{\link[Benchmarking]{dea.add}} for RTS codes;
#'   \code{\link[rminer]{Importance}} for SA settings;
#'   \code{\link[fastshap]{explain}} for SHAP;
#'   \code{\link[iml]{FeatureImp}} for permutation importance (PI).
#'
#' @note For any ambiguity on argument semantics, please refer to the official
#'   documentation of \pkg{rminer} (SA), \pkg{fastshap} (SHAP), and \pkg{iml} (PI).
#'
#' @keywords internal
#' @noRd
#'

validate_parametes_PEAXAI_global_importance <- function(
    data, x, y, final_model, background, target, importance_method
) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }

  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Final model ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(final_model) || is.null(final_model) || !inherits(final_model, "train")) {
    stop("'final_model' must be a caret::train() object (class 'train').", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # background and target ------------------------------------------------------
  # ----------------------------------------------------------------------------
  allowed_bt <- c("train","real")
  if (!is.character(background) || length(background) != 1L || !(background %in% allowed_bt)) {
    stop("'background' must be 'train' or 'real'.", call. = FALSE)
  }
  if (!is.character(target) || length(target) != 1L || !(target %in% allowed_bt)) {
    stop("'target' must be 'train' or 'real'.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Importance method ----------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(importance_method) || !is.list(importance_method)) {
    stop("'importance_method' must be a list.", call. = FALSE)
  }

  name <- importance_method[["name"]]

  if (!is.character(name) || length(name) != 1L || !(name %in% c("SA","SHAP","PI"))) {
    stop("'importance_method$name' must be one of: 'SA', 'SHAP', 'PI'.", call. = FALSE)
  }

  if (identical(name, "SA")) {
    # rminer
    if (!is.null(importance_method[["method"]]) &&
        !importance_method[["method"]] %in% c("1D-SA","sens","DSA","MSA","CSA")) {
      stop("'importance_method$method' (SA/rminer) must be one of: '1D-SA','sens','DSA','MSA','CSA'.", call. = FALSE)
    }
    if (!is.null(importance_method[["measures"]]) &&
        !all(importance_method[["measures"]] %in% c("AAD","gradient","variance","range"))) {
      stop("'importance_method$measures' (SA/rminer) must be in: 'AAD','gradient','variance','range'.", call. = FALSE)
    }
    if (!is.null(importance_method[["levels"]])) {
      lv <- importance_method[["levels"]]
      if (!is.numeric(lv) || length(lv) != 1L || lv < 2) {
        stop("'importance_method$levels' (SA/rminer) must be a single numeric >= 2.", call. = FALSE)
      }
    }
    if (!is.null(importance_method[["baseline"]])) {
      base <- importance_method[["baseline"]]
      base_ok <- (is.character(base) && base %in% c("mean","media")) || is.data.frame(base)
      if (!base_ok) {
        stop("'importance_method$baseline' (SA/rminer) must be 'mean', 'media', or a data.frame.", call. = FALSE)
      }
    }
  } else if (identical(name, "SHAP")) {
    # fastshap
    nsim <- importance_method[["nsim"]]
    if (is.null(nsim) || !is.numeric(nsim) || length(nsim) != 1L || nsim <= 0) {
      stop("'importance_method$nsim' (SHAP/fastshap) must be a positive integer.", call. = FALSE)
    }
  } else if (identical(name, "PI")) {
    # iml
    nrep <- importance_method[["n.repetitions"]]
    if (is.null(nrep) || !is.numeric(nrep) || length(nrep) != 1L || nrep <= 0) {
      stop("'importance_method$n.repetitions' (PI/iml) must be a positive integer.", call. = FALSE)
    }
  }

  invisible(NULL)
}

#' @title Validate input parameters for PEAXAI targets
#'
#' @description
#' Validates arguments prior to constructing target sets with \code{PEAXAI_targets}.
#' It checks data containers, input/output index ranges, the trained model object,
#' efficiency thresholds, and the directional vector specification (scope, baseline,
#' and relative importance), as well as grid/expansion and bound parameters.
#'
#' @param data A \code{data.frame} or \code{matrix} containing all variables (rows = DMUs).
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param final_model A trained \code{caret} model, i.e., an object of class \code{"train"}.
#' @param efficiency_thresholds Numeric vector of efficiency thresholds (each strictly in \code{(0,1)}).
#' @param directional_vector A \code{list} with fields:
#'   \itemize{
#'     \item \code{relative_importance}: numeric, non-negative weights for inputs; length \code{= length(x)}; not all zero.
#'     \item \code{scope}: \code{"global"} or \code{"local"} (currently only \code{"global"} supported).
#'     \item \code{baseline}: one of \code{"mean"}, \code{"median"}, \code{"self"}, \code{"ones"}.
#'   }
#' @param n_expand Numeric expansion factor (e.g., \code{0.5}); must be a single non-negative number.
#' @param n_grid Integer number of grid points used in search/refinement; must be a single integer >= 2.
#' @param max_y Numeric non-negative upper bound multiplier for outputs (domain-specific).
#' @param min_x Numeric non-negative lower bound multiplier for inputs (domain-specific).
#'
#' @details
#' The function throws an informative error on the first detected violation. On success,
#' it returns \code{NULL} invisibly. If in doubt about modelling choices behind
#' \code{relative_importance}/\code{scope}/\code{baseline}, consult your method section
#' and keep them consistent across experiments.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @seealso \code{\link[caret]{train}} for the \code{"train"} object.
#'
#' @keywords internal
#' @noRd
#'

validate_parametes_PEAXAI_targets <- function(
    data, x, y, final_model,
    efficiency_thresholds, directional_vector,
    n_expand, n_grid, max_y, min_x
) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x e y ----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }

  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # final model ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(final_model) || is.null(final_model) || !inherits(final_model, "train")) {
    stop("'final_model' must be a caret::train() object (class 'train').", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Efficiency thresholds ------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(efficiency_thresholds) || is.null(efficiency_thresholds) ||
      !is.numeric(efficiency_thresholds) || length(efficiency_thresholds) < 1L) {
    stop("'efficiency_thresholds' must be a non-empty numeric vector.", call. = FALSE)
  }
  if (any(efficiency_thresholds <= 0 | efficiency_thresholds >= 1)) {
    stop("'efficiency_thresholds' values must lie strictly in (0,1).", call. = FALSE)
  }
  # # Optional monotonicity check:
  # if (is.unsorted(efficiency_thresholds, strictly = TRUE)) {
  #   stop("'efficiency_thresholds' must be strictly increasing.", call. = FALSE)
  # }

  # ----------------------------------------------------------------------------
  # Directional vector ---------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(directional_vector) || !is.list(directional_vector)) {
    stop("'directional_vector' must be a list.", call. = FALSE)
  }
  # relative_importance
  ri <- directional_vector[["relative_importance"]]

  if (!is.data.frame(ri)) {
    ri <- as.data.frame(t(ri))

    if (ncol(ri) == 1) {
      ri <- as.data.frame(t(ri))
    }
  }

  if (is.null(ri) || !is.data.frame(ri) || any(ri < 0) || length(ri) != length(c(x, y))) {

    stop("'directional_vector$relative_importance' must be a non-negative numeric vector of length length(c(x,y)).",
         call. = FALSE)
  }
  if (sum(ri) == 0) {
    stop("'directional_vector$relative_importance' cannot be the all-zero vector.", call. = FALSE)
  }
  if (abs(sum(ri) - 1) > 1e-8) {
    warning("'directional_vector$relative_importance' does not sum to 1; proceeding without normalization.", call. = FALSE)
  }
  # scope
  scope <- directional_vector[["scope"]]
  if (is.null(scope) || !is.character(scope) || length(scope) != 1L ||
      !(scope %in% c("global","local"))) {
    stop("'directional_vector$scope' must be 'global' or 'local'.", call. = FALSE)
  }
  if (identical(scope, "local")) {
    warning("Support for 'local' scope is not yet implemented; using 'global' semantics.", call. = FALSE)
  }
  # baseline
  baseline <- directional_vector[["baseline"]]
  if (is.null(baseline) || !is.character(baseline) || length(baseline) != 1L ||
      !(baseline %in% c("mean","median","self","ones"))) {
    stop("'directional_vector$baseline' must be one of: 'mean','median','self','ones'.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # n_expand and n_grid --------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.numeric(n_expand) || length(n_expand) != 1L || n_expand < 0) {
    stop("'n_expand' must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(n_grid) || length(n_grid) != 1L || n_grid < 2 || n_grid != as.integer(n_grid)) {
    stop("'n_grid' must be a single integer >= 2.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # max_y and min_x ------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.numeric(max_y) || length(max_y) != 1L || max_y < 0) {
    stop("'max_y' must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(min_x) || length(min_x) != 1L || min_x < 0) {
    stop("'min_x' must be a single non-negative numeric value.", call. = FALSE)
  }

  invisible(NULL)
}

#' @title Validate input parameters for PEAXAI ranking
#'
#' @description
#' Validates arguments prior to computing a PEAXAI-based ranking. It checks data
#' containers, input/output index ranges, the trained model object, efficiency
#' thresholds, the targets object consistency, and the ranking basis.
#'
#' @param data A \code{data.frame} or \code{matrix} with all variables (rows = DMUs).
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param final_model A trained \code{caret} model, i.e., an object of class \code{"train"}.
#' @param efficiency_thresholds Numeric vector of efficiency thresholds (each strictly in \code{(0,1)}),
#'   usually strictly increasing (e.g., \code{seq(0.75, 0.95, 0.1)}).
#' @param targets Target specification produced by \code{PEAXAI_targets()} (either a \code{data.frame}
#'   or a \code{list}). Minimal consistency checks are performed (see Details).
#' @param rank_basis Character scalar indicating the basis for ranking. Must be one of
#'   \code{"predicted"} or \code{"attainable"}.
#'
#' @details
#' \strong{Targets consistency:}
#' \itemize{
#'   \item If \code{targets} is a \code{data.frame}, it must be non-empty. If it has a column
#'         named \code{efficiency_threshold} (or \code{threshold}), all values must be a subset
#'         of \code{efficiency_thresholds}.
#'   \item If \code{targets} is a \code{list}, it must be non-empty. If it is named, and names
#'         \emph{look like} thresholds, they must be a subset of \code{efficiency_thresholds}.
#'         If unnamed, its length should reasonably match either \code{length(efficiency_thresholds)}
#'         (targets-per-threshold) or \code{nrow(data)} (targets-per-DMU). In ambiguous cases a
#'         warning is issued.
#' }
#' The function throws an error on the first violation and returns \code{NULL} invisibly otherwise.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @seealso \code{\link[caret]{train}} for the \code{"train"} object; \code{PEAXAI_targets()}.
#'
#' @keywords internal
#' @noRd
#'

validate_parametes_PEAXAI_ranking <- function(
    data, x, y, final_model,
    efficiency_thresholds, targets, rank_basis
) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }

  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Final model ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(final_model) || is.null(final_model) || !inherits(final_model, "train")) {
    stop("'final_model' must be a caret::train() object (class 'train').", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Efficiency thresholds ------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (rank_basis == "attainable") {
    if (missing(efficiency_thresholds) || is.null(efficiency_thresholds) ||
        !is.numeric(efficiency_thresholds) || length(efficiency_thresholds) < 1L) {
      stop("'efficiency_thresholds' must be a non-empty numeric vector.", call. = FALSE)
    }
    if (any(efficiency_thresholds <= 0 | efficiency_thresholds >= 1)) {
      stop("'efficiency_thresholds' values must lie strictly in (0,1).", call. = FALSE)
    }
  }

  # ----------------------------------------------------------------------------
  # Targets --------------------------------------------------------------------
  # ----------------------------------------------------------------------------

  # if (missing(targets) || is.null(targets)) {
  #   stop("'targets' must be provided (result of PEAXAI_targets()).", call. = FALSE)
  # }

  if (!is.null(targets)) {

    if (!is.list(targets)) {
      stop("'targets' must be a list. It is more easy pass the object provided by 'PEAXAI_targets()'", call. = FALSE)
    }

    if (length(targets) == 0L) {
      stop("'targets' list is empty.", call. = FALSE)
    }

    for (eff_i in names(targets)) {
      if (!all(names(targets[[eff_i]][["counterfactual_dataset"]]) == names(data)[c(x,y)])) {
        stop("names in 'target' are not the same than in 'data'",  call. = FALSE)
      }
    }
  }
  # } else {
  #   # stop("'targets' must be a list. It more easy pass the object provided by 'PEAXAI_targets'", call. = FALSE)
  # }

  # ----------------------------------------------------------------------------
  # Rank basis -----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.character(rank_basis) || length(rank_basis) != 1L ||
      !(rank_basis %in% c("predicted","attainable"))) {
    stop("'rank_basis' must be 'predicted' or 'attainable'.", call. = FALSE)
  }

  if (rank_basis == "attainable") {
    if (is.null(targets) || !is.list(targets)) {
      stop(paste0("If 'rank_basis' is 'attainable', it is necesary to pas 'targets' argument, more easily provided by PEAXAI_tragets()"))
    }
  }

  invisible(NULL)
}

#' @title Validate input parameters for PEAXAI peer analysis
#'
#' @description
#' Validates arguments prior to computing PEAXAI peers. It checks data containers,
#' input/output index ranges, the trained model object, efficiency thresholds,
#' and the weighting configuration for peer distance.
#'
#' @param data A \code{data.frame} or \code{matrix} with all variables (rows = DMUs).
#' @param x Numeric vector of column indices in \code{data} corresponding to input variables.
#' @param y Numeric vector of column indices in \code{data} corresponding to output variables.
#' @param final_model A trained \code{caret} model, i.e., an object of class \code{"train"}.
#' @param efficiency_thresholds Numeric vector of efficiency thresholds (each strictly in \code{(0,1)}),
#'   typically strictly increasing (e.g., \code{seq(0.75, 0.95, 0.1)}).
#' @param weighted Logical scalar; if \code{TRUE}, peer distances are weighted by
#'   \code{relative_importance}. If \code{FALSE}, \code{relative_importance} is ignored.
#' @param relative_importance When \code{weighted = TRUE}, a one-row \code{data.frame} with
#'   non-negative weights for input variables. Column names should match \code{colnames(data)[x]}.
#'   If unnamed, the number of columns must equal \code{length(x)}. Not available per-unit.
#'
#' @details
#' The function throws an informative error on the first detected violation and
#' returns \code{NULL} invisibly on success.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @seealso \code{\link[caret]{train}} for the \code{"train"} object.
#'
#' @keywords internal
#' @noRd
#'

validate_parametes_PEAXAI_peer <- function(
    data, x, y, final_model,
    efficiency_thresholds, weighted = FALSE, relative_importance = NULL
) {

  # ----------------------------------------------------------------------------
  # Data -----------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("'data' has zero rows or zero columns.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # x and y --------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!(is.numeric(x) && is.numeric(y))) {
    stop("'x' and 'y' must be numeric index vectors.", call. = FALSE)
  }

  if (any(x %in% y)) {
    stop("'x' and 'y' must not overlap.", call. = FALSE)
  }

  p <- ncol(data)
  if (any(x < 1 | x > p) || any(y < 1 | y > p)) {
    stop("'x' and/or 'y' are out of column range.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Final mpdel ----------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(final_model) || is.null(final_model) || !inherits(final_model, "train")) {
    stop("'final_model' must be a caret::train() object (class 'train').", call. = FALSE)
  }

  # ----------------------------------------------------------------------------
  # Efficiency thresholds ------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (missing(efficiency_thresholds) || is.null(efficiency_thresholds) ||
      !is.numeric(efficiency_thresholds) || length(efficiency_thresholds) < 1L) {
    stop("'efficiency_thresholds' must be a non-empty numeric vector.", call. = FALSE)
  }
  if (any(efficiency_thresholds <= 0 | efficiency_thresholds >= 1)) {
    stop("'efficiency_thresholds' values must lie strictly in (0,1).", call. = FALSE)
  }
  # if (is.unsorted(efficiency_thresholds, strictly = TRUE)) {
  #   stop("'efficiency_thresholds' must be strictly increasing.", call. = FALSE)
  # }


  # ----------------------------------------------------------------------------
  # Weighted -------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  if (!is.logical(weighted) || length(weighted) != 1L) {
    stop("'weighted' must be a single logical (TRUE/FALSE).", call. = FALSE)
  }

  if (isTRUE(weighted)) {
    if (is.null(relative_importance) || !is.data.frame(relative_importance)) {
      stop("'relative_importance' must be a one-row data.frame when 'weighted = TRUE'.", call. = FALSE)
    }

    # --------------------------------------------------------------------------
    # Relative importance ------------------------------------------------------
    # --------------------------------------------------------------------------
    if (nrow(relative_importance) != 1L) {
      stop("'relative_importance' must have exactly one row (global weights only).", call. = FALSE)
    }

    # Names and alignment to inputs
    input_names <- colnames(as.data.frame(data))[c(x,y)]
    ri_names    <- colnames(relative_importance)

    if (!is.null(ri_names) && any(nzchar(ri_names))) {
      # Must be a subset and aligned (we'll reorder internally if needed)
      if (!all(ri_names %in% input_names)) {
        browser()
        stop("'relative_importance' column names must match input names colnames(data)[c(x,y)].", call. = FALSE)
      }
      # Ensure we have weights for all inputs
      if (!all(input_names %in% ri_names)) {
        stop("'relative_importance' must provide weights for all inputs in colnames(data)[c(x,y)].", call. = FALSE)
      }
    } else {
      # Unnamed: must match by position
      if (ncol(relative_importance) != length(c(x,y))) {
        stop("Unnamed 'relative_importance' must have ncol == length(c(x,y)).", call. = FALSE)
      }
      # # Assign input names for clarity downstream
      # colnames(relative_importance) <- input_names
    }

    # Numeric, non-negative, finite, not all zero
    vals <- as.numeric(relative_importance[1, , drop = TRUE])
    if (any(!is.finite(vals)) || any(vals < 0)) {
      stop("'relative_importance' weights must be finite and non-negative.", call. = FALSE)
    }
    if (all(vals == 0)) {
      stop("'relative_importance' cannot be the all-zero vector.", call. = FALSE)
    }

    # (Opcional) Aviso si no suma 1
    if (abs(sum(vals) - 1) > 1e-8) {
      warning("'relative_importance' does not sum to 1; proceeding without normalization.", call. = FALSE)
    }
  } else {
    # weighted == FALSE -> ignore relative_importance silently
    # (no-op)
  }

  invisible(NULL)
}



#' @title Prepare Data and Handle Errors
#'
#' @description This function arranges the data in the required format and displays some error messages.
#'
#' @param data A \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column indexes of input variables in \code{data}.
#' @param y Column indexes of output variables in \code{data}.
#'
#' @importFrom caret trainControl
#'
#' @return It returns a \code{matrix} in the required format and displays some error messages.

preprocessing <- function (
    data, x, y
) {

  # x and y well / bad introduced

  cols <- 1:length(data)
  if (!(all(x %in% cols))) {
    stop("x index(es) are not in data.")

    if (!(all(y %in% cols))) {
      stop("y index(es) are not in data.")
    }
  }

  # (i) data.frame, (ii) list with variables, (iii) matrix

  # data.frame format to deal with classes
  if (is.list(data) && !is.data.frame(data)) {

    # data names?
    ifelse(is.null(names(data)),
           var_names <- 1:length(data), # if not 1:x
           var_names <- names(data)
    )

    data <- matrix(unlist(data), ncol = length(var_names), byrow = F)
    colnames(data) <- var_names

  } else if (is.matrix(data) || is.data.frame(data)) {
    data <- data.frame(data)
  }

  # Classes
  varClass <- unlist(sapply(data, class))

  # Output classes
  outClass <- varClass[y] %in% c("numeric", "double", "integer")

  # Error
  if (!all(outClass)){
    stop(paste(names(data)[y][!outClass][1], "is not a numeric or integer vector"))
  }

  # Input classes
  # Ordered --> numeric
  for (i in x){
    if (is.ordered(data[, i])) {
      data[, i] <- as.numeric(data[, i])
    }
  }

  # Define classes again
  varClass <- unlist(sapply(data, class))

  inpClass <- varClass[x] %in% c("numeric", "double", "integer")

  # Error
  if (!all(inpClass)){
    stop(paste(names(data)[x][!inpClass][1], "is not a numeric, integer or ordered vector"))
  }

  data <- data[, c(x, y)]

  return(as.matrix(data))
}
