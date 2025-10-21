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
imbalance_rate <- seq(0.2, 0.9, 0.1)

# ML metric to maximize during cross-validation
metric_priority <- c("F1", "Balanced Accuracy") #  Balanced Accuracy, F1, PR-AUC, ROC

# parameters for controlling the training process
trControl <- list( # trainControl
  method = "test_set", # none test_set cv
  number = NULL, # 5 10
  test_hold_out = 0.2# 0.2 NULL
)

# Split data to validate on samples not used in training
hold_out <- 0.2

# Set seed to reproduce results
seed <- 1
set.seed(seed)

#-------------------------------------------------------------------------------
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
  "rf" = list(
    tuneGrid = expand.grid(mtry = c(2, 3, 4, 5)),
    ntree = 5
  )
  # "glm" = list(
  #   family = binomial(link = "logit"),
  #   direction = "both",
  #   weights = NULL, # data.frame(w0 = 0.5, w1 = 2), "dinamic"
  #   trace = FALSE
  # )
)

#-------------------------------------------------------------------------------
# PEAXAI training --------------------------------------------------------------
# ------------------------------------------------------------------------------
PEAXAI_models <- PEAXAI_fitting(
  data = data,
  x = x,
  y = y,
  RTS = RTS,
  imbalance_rate = imbalance_rate,
  methods = methods,
  trControl = trControl,
  metric_priority = metric_priority,
  hold_out = hold_out
)

#-------------------------------------------------------------------------------
# PEAXAI importance features ---------------------------------------------------
# ------------------------------------------------------------------------------
# XAI method to determine relative importance of variables
importance_method <- data.frame(
  name = "SHAP", # "SA", "SHAP", "PI"
  nsim = 200 # only SHAP
  # methods = "1D-SA", # only SA  c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  # measures_SA = "AAD", # only SA  c("AAD", "gradient", "variance", "range")
  # levels = 7 # only SA
)

PEAXAI_global_importance <- PEAXAI_global_importance(
  data = data,
  x = x,
  y = y,
  final_model = PEAXAI_models[["best_models"]][["nnet"]],
  background = "train",
  target = "train",
  importance_method = importance_method
)

#-------------------------------------------------------------------------------
# PEAXAI targets ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# Efficiency thresholds
efficiency_thresholds <- seq(0.75, 0.95, 0.1) # seq(0.75, 0.95, 0.1)

directional_vector <- list(
  mode = "importance",     # "ones" | "mean" | "self" | "importance" | "custom",
  imp_level = "global",         # "global" # not yet | "local"
  scope = "both",           # "inputs" | "outputs" | "both"
  baseline  = "mean"           # "none" | "mean" | "median"
)

devtools::document()
devtools::load_all()
PEAXAI_analysis <- PEAXAI_targets(
  data = data,
  x = x,
  y = y,
  directional_vector = directional_vector,
  imp_global = PEAXAI_global_importance,
  final_model = PEAXAI_models[["best_models"]][["nnet"]],
  efficiency_thresholds = efficiency_thresholds
)

#-------------------------------------------------------------------------------
# PEAXAI ranking  --------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::document()
devtools::load_all()
PEAXAI_analysis <- PEAXAI_ranking(
  data = data,
  x = x,
  y = y,
  final_model = PEAXAI_models[["best_models"]][["nnet"]],
  efficiency_thresholds = efficiency_thresholds
)




#-------------------------------------------------------------------------------
# PEAXAI peer ------------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::document()
devtools::load_all()
PEAXAI_analysis <- PEAXAI_peer(
  data = data,
  x = x,
  y = y,
  final_model = PEAXAI_models[["best_models"]][["nnet"]],
  efficiency_thresholds = efficiency_thresholds
)
