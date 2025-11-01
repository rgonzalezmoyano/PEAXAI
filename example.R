################################################################################
####################### EXAMPLE OF USE 'PEAXAI' ################################
################################################################################

# ------------------------------------------------------------------------------
# Load PEAXAI ------------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::document()
devtools::load_all()
options(error = NULL)
options(error = recover)
# ------------------------------------------------------------------------------
# Load dataset -----------------------------------------------------------------
# ------------------------------------------------------------------------------
# load data
data("firms", package = "PEAXAI")
# filter to valencian comunity
data <- firms[firms$autonomous_community == "Comunidad Valenciana",]
# data <- firms
# delete original dataframe
rm(firms)

# ------------------------------------------------------------------------------
# Parameters to PEAXAI models fitting ------------------------------------------
# ------------------------------------------------------------------------------
# x and y indexes
x <- c(1:4)
y <- c(5)

# Returns to scale assumption
RTS <- "vrs"

# Addressing target imbalance by specifying resampling ratios, using SMOTE units.
imbalance_rate <- seq(0.95, 0.95, 0.05) #seq(0.2, 0.5, 0.1)

# ML metric to maximize during cross-validation
metric_priority <- c("F1", "Balanced Accuracy", "ROC-AUC") #  Balanced Accuracy, F1, PR-AUC, ROC

# parameters for controlling the training process
trControl <- list(
  method = "cv",
  number = 5
)

# Split data to validate on samples not used in training
hold_out <- NULL

# Set seed to reproduce results
seed <- 1
set.seed(seed)

# ------------------------------------------------------------------------------
# ML models --------------------------------------------------------------------
# ------------------------------------------------------------------------------
methods <- list(
  "nnet" = list(
    tuneGrid = expand.grid(
      size = c(1, 5, 10, 20),
      decay = 10^seq(-5, -1, by = 1)
    ),
    maxit = 100,
    preProcess = c("center", "scale"),
    # # --- arguments nnet ---
    entropy = TRUE,
    skip = TRUE,
    maxit = 1000,
    MaxNWts = 100000,
    trace = FALSE,
    weights = NULL
  ),
  "svmPoly" = list(
    tuneGrid = expand.grid(
      degree = c(2,3),
      scale = 10^seq(-3, 1, by = 1),
      C = c(0.5, 1, 2, 4, 8, 16)
    )
  ),
  "glm" = list(
    weights = "dinamic" # data.frame(w0 = 0.5, w1 = 2), "dinamic"
  )
)

# ------------------------------------------------------------------------------
# PEAXAI training --------------------------------------------------------------
# ------------------------------------------------------------------------------
models <- PEAXAI_fitting(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  imbalance_rate = imbalance_rate,
  methods = methods,
  trControl = trControl,
  metric_priority = metric_priority,
  hold_out = hold_out,
  verbose = TRUE
)

for (model in names(models$best_model_fit)) {


  # ------------------------------------------------------------------------------
  # PEAXAI importance features ---------------------------------------------------
  # ------------------------------------------------------------------------------
  # XAI method to determine relative importance of variables
  importance_method <- list(
    name = "SA", # "SA", "SHAP", "PI"
    nsim = 200, # only SHAP
    method = "1D-SA", # only SA  c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures = "AAD", # only SA  c("AAD", "gradient", "variance", "range")
    levels = 7, # only SA
    baseline = "mean", # only SA
    n.repetitions = 50 # only PI
  )

  relative_importance <- PEAXAI_global_importance(
    data = data,
    x = x,
    y = y,
    final_model = models[["best_model_fit"]][[model]],
    background = "train",
    target = "train",
    importance_method = importance_method
  )

  # ------------------------------------------------------------------------------
  # PEAXAI targets ---------------------------------------------------------------
  # ------------------------------------------------------------------------------
  # Efficiency thresholds
  efficiency_thresholds <- seq(0.5, 1, 0.1) # seq(0.75, 0.95, 0.1)

  directional_vector <- list(
    relative_importance = relative_importance,
    scope = "global",           # "global" # not yet | "local"
    baseline  = "mean"        # "mean" | "median" | "self" | "ones"
  )

  targets <- PEAXAI_targets(
    data = data,
    x = x,
    y = y,
    final_model = models[["best_model_fit"]][[model]],
    efficiency_thresholds = efficiency_thresholds,
    directional_vector = directional_vector,
    n_expand = 0.5,
    n_grid = 50,
    max_y = 2,
    min_x = 1
  )

  # ------------------------------------------------------------------------------
  # PEAXAI ranking  --------------------------------------------------------------
  # ------------------------------------------------------------------------------
  ranking <- PEAXAI_ranking(
    data = data,
    x = x,
    y = y,
    final_model = models[["best_model_fit"]][[model]],
    efficiency_thresholds = efficiency_thresholds,
    targets = targets,
    rank_basis = "predicted" # | "predicted" "attainable"
  )

  ranking_plot <- as.data.frame(ranking$probability_predicted)
  names(ranking_plot) <- "probability_predicted"
  # rm(ranking_plot)
  ranking <- PEAXAI_ranking(
    data = data,
    x = x,
    y = y,
    final_model = models[["best_model_fit"]][[model]],
    efficiency_thresholds = efficiency_thresholds,
    targets = targets,
    rank_basis = "attainable" # | "predicted" "attainable"
  )
  ranking_plot$`0.75` <- ranking$`0.75`$probability_target
  ranking_plot$`0.85` <- ranking$`0.85`$probability_target
  ranking_plot$`0.95` <- ranking$`0.95`$probability_target
  ranking_plot$position <- 1:nrow(ranking_plot)

  # predictions <- predict(models$best_model_fit$glm, newdata = data[,c(x, y)], type = "prob")[,1]
  # which(predictions > 0.5)
  # length(which(predictions > 0.5))
  # efficient DMUs DEA
  #  1  2  3  9    17 18 20       26    36       46 56    62       92 93 97
  # efficient DMUs NN
  #  1  2  3  9    17 18 20       26    36       46 56    62       92 93 97
  # efficient DMUs SVM
  #  1  2  3             20
  # efficient DMUs glm
  #  1  2  3  9 15 17 18 20 22 25 26 31 36 43 44 46 56 59 62 75 83


  # # Recall SVM
  # recall_svm <- 4/15 # 0.2666667
  #
  # # Precision SVM
  # precision_svm <- 1
  #
  # # F1 SVM
  # f1_svm <- (2*recall_svm*precision_svm) / (recall_svm+precision_svm)
  # # 0.4210526
  #
  # # Balance Accuracy
  # BA_svm <- (recall_svm + 1) / 2

  ggplot(data = ranking_plot, aes(x = position)) +

    geom_area(aes(y = `0.95`), fill = "black", alpha = 0.10) +
    geom_area(aes(y = `0.85`), fill = "black", alpha = 0.20) +
    geom_area(aes(y = `0.75`), fill = "black", alpha = 0.40) +


    geom_area(aes(y = probability_predicted), fill = "black", alpha = 0.7) +

    geom_line(aes(y = probability_predicted),
              linewidth = 1.1) +
    geom_line(aes(y = `0.75`),
              linetype = "dotted") +
    geom_line(aes(y = `0.85`),
              linetype = "dotted") +
    geom_line(aes(y = `0.95`),
              linetype = "dotted") +

    labs(
      title = sprintf("%s — DMUs: %d", model, nrow(data)),
      x = "Position",
      y = "Probability"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid.minor = element_blank()
    )

  # ------------------------------------------------------------------------------
  # PEAXAI peer ------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  peers <- PEAXAI_peer(
    data = data,
    x = x,
    y = y,
    final_model = models[["best_model_fit"]][[model]],
    efficiency_thresholds = efficiency_thresholds,
    weighted = FALSE,
    relative_importance = relative_importance
  )

}
