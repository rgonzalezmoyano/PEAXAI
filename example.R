################################################################################
####################### EXAMPLE OF USE 'PEAXAI' ################################
################################################################################

# ------------------------------------------------------------------------------
# Load PEAXAI ------------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::document()
devtools::load_all()

# ------------------------------------------------------------------------------
# Load dataset -----------------------------------------------------------------
# ------------------------------------------------------------------------------
# load data
data("firms", package = "PEAXAI")

# filter to valencian comunity
# data <- firms[firms$autonomous_community == "Comunidad Valenciana",]
data <- firms

# ------------------------------------------------------------------------------
# Parameters to PEAXAI models fitting ------------------------------------------
# ------------------------------------------------------------------------------
# x and y indexes
x <- c(1:4)
y <- c(5)

# Returns to scale assumption
RTS <- "vrs"

# Addressing target imbalance by specifying resampling ratios, using SMOTE units.
balance_data <- seq(0.1, 0.9, 0.1)

# ML metric to maximize during cross-validation
metric = "F1" #  Balanced Accuracy, F1, PR-AUC, ROC

# parameters for controlling the training process
trControl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  savePredictions = "all",
  allowParallel = FALSE
)

# trControl <- trainControl(
#   method = "none"
# )

# Split data to validate on samples not used in training
validation_hold_out <- 0.4
test_hold_out <- NULL # 0.2

# Set seed to reproduce results
seed <- 1
set.seed(seed)

#-------------------------------------------------------------------------------
# ML models --------------------------------------------------------------------
# ------------------------------------------------------------------------------
methods <- list(
  "nnet" = list(
    tuneGrid = expand.grid(
      size = c(1, 3, 5, 7, 10, 15),
      decay = 10^seq(-5, -1, by = 1)
    ),
    maxit = 100,
    preProcess = c("center", "scale"),
    # # --- arguments nnet ---
    entropy   = TRUE,
    skip      = TRUE,
    maxit     = 1000,
    MaxNWts   = 100000,
    trace     = FALSE,
    weights   = NULL
  ),
  "rf" = list(
    tuneGrid = expand.grid(mtry = c(2, 3, 4, 5)),
    ntree = 5
  ),
  "glm" = list(
    family = binomial(link = "logit"),
    direction = "both",
    weights = NULL,
    trace = FALSE
  )
)

# model result
PEAXAI_models <- PEAXAI_fitting(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  balance_data = balance_data,
  methods = methods,
  trControl = trControl,
  metric = metric,
  hold_out = validation_hold_out,
  test_hold_out = test_hold_out
)

#
# PEAXAI_models$best_performance
#
# # a1 <- PEAXAI_models$best_performance
# a2 <- PEAXAI_models$best_performance
# a3 <- PEAXAI_models$best_performance
# # XAI method to determine relative importance of varaibles
# importance_method <- "SHAP"
#
# # Efficiency thresholds
# efficiency_thresholds <- seq(0.75, 0.95, 0.1)
#
# PEAXAI_analisys <- PEAXAI_analisys(
#   data = PEAXAI_models$dataset_labeled,
#   x = x,
#   y = y,
#   models = PEAXAI_models,
#   importance_method = importance_method,
#   scenarios = efficiency_thresholds
# )
