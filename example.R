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
imbalance_rate <- seq(0.2, 0.5, 0.1)

# ML metric to maximize during cross-validation
metric_priority <- c("F1", "Balanced Accuracy") #  Balanced Accuracy, F1, PR-AUC, ROC

# parameters for controlling the training process
trControl <- list( # trainControl
  method = "cv", # none test_set cv
  number = 5, # 5 10
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
    ntree = 500
  ),
  "glm" = list(
    family = binomial(link = "logit"),
    direction = "both",
    weights = NULL, # data.frame(w0 = 0.5, w1 = 2), "dinamic"
    trace = FALSE
  )
)

#-------------------------------------------------------------------------------
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
  hold_out = hold_out
)

model <- "rf"

#-------------------------------------------------------------------------------
# PEAXAI importance features ---------------------------------------------------
# ------------------------------------------------------------------------------
# XAI method to determine relative importance of variables
importance_method <- list(
  name = "SHAP", # "SA", "SHAP", "PI"
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
  final_model = models[["best_models"]][[model]],
  background = "train",
  target = "train",
  importance_method = importance_method
)

#-------------------------------------------------------------------------------
# PEAXAI targets ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# Efficiency thresholds
efficiency_thresholds <- 0.75 # seq(0.75, 0.95, 0.1)

directional_vector <- list(
  relative_importance = relative_importance,
  scope = "global",           # "global" # not yet | "local"
  baseline  = "self"        # "mean" | "median" | "self" | "ones"
)
devtools::document()
devtools::load_all()

targets <- PEAXAI_targets(
  data = data,
  x = x,
  y = y,
  final_model = models[["best_models"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  directional_vector = directional_vector,
  n_expand = 0.5,
  n_grid = 300,
  max_y = 2,
  min_x = 1
)

#-------------------------------------------------------------------------------
# PEAXAI ranking  --------------------------------------------------------------
# ------------------------------------------------------------------------------
ranking <- PEAXAI_ranking(
  data = data,
  x = x,
  y = y,
  final_model = models[["best_models"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  targets = targets,
  rank_basis = "predicted" # | "observed" "attainable"
)
plot(ranking$probability_predicted)

# tail(round(ranking$`0.75`,3), 10)

#-------------------------------------------------------------------------------
# PEAXAI peer ------------------------------------------------------------------
# ------------------------------------------------------------------------------
peers <- PEAXAI_peer(
  data = data,
  x = x,
  y = y,
  final_model = models[["best_models"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  weighted = FALSE,
  relative_importance = relative_importance
)
