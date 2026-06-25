################################################################################
######################## EXAMPLE OF USE 'PEAXAI Z' #############################
################################################################################

# ------------------------------------------------------------------------------
# Parameters of save plots -----------------------------------------------------
# ------------------------------------------------------------------------------
width <- 30
height <- 18

save_plots <- FALSE
size_point <- 3
size_names <- 5

# ------------------------------------------------------------------------------
# Load library -----------------------------------------------------------------
# ------------------------------------------------------------------------------
# load PEAXAI
devtools::document()
devtools::load_all()

# load others
# install.packages("PEAXAI")
library(ggplot2)
library(scales)
library(openxlsx)
# library(PEAXAI)

# ------------------------------------------------------------------------------
# Load dataset -----------------------------------------------------------------
# ------------------------------------------------------------------------------
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/PEAXAI/dataPISA_2022.Rdata")
# load("C:/Users/Ricardo/Documents/Doctorado EOMA/AAA Papers&Chapters/02_thesis_PEAXAI Z/dataPISA_2022.Rdata")
data <- dataPISA_2022

# ------------------------------------------------------------------------------
# Parameters to PEAXAI models fitting ------------------------------------------
# ------------------------------------------------------------------------------
# x and y indexes
x <- c(2:4)
y <- c(5:7)
# data <- data[data$CNT == "ESP",]
# data <- data[1:5000,]

# environment variables
z_numeric <- c(8:12)
z_numeric <- NULL

z_factor <- c(13:18) # 18
# z_factor <- c(18)
if (!is.null(z_factor)) {
  data[, z_factor] <- lapply(data[, z_factor, drop = FALSE], as.factor)
}

# Returns to scale assumption
RTS <- "vrs"

# Bootstrap DEA conditional
B <- 1

# number of units to be included in the reference set
m <- NULL

# Addressing target imbalance by specifying resampling ratios, using SMOTE units.
imbalance_rate <- seq(0.1, 0.5, 0.05) #

# ML metric to maximize during cross-validation
metric_priority <- c("Balanced_Accuracy", "F1", "ROC_AUC")

# parameters for controlling the training process
trControl <- list(
  method = "cv",
  number = 5
)

# Split data to validate on samples not used in training
hold_out <- NULL

# Set seed to reproduce results
seed <- 314

# ------------------------------------------------------------------------------
# ML models --------------------------------------------------------------------
# ------------------------------------------------------------------------------
methods <- list(
  "nnet" = list(
    tuneGrid = expand.grid(
      size  = c(1, 3, 5, 10),
      decay = 10^seq(-5, -1, by = 1)
    ),
    maxit = 2000,
    preProcess = c("center", "scale"),
    entropy = TRUE,
    skip = FALSE,
    MaxNWts = 500000,
    trace = FALSE,
    weights = NULL
  ),
  "svmPoly" = list(
    tuneGrid = expand.grid(
      degree = c(1, 2),
      scale  = c(0.1, 1),
      C      = c(0.05, 0.1, 0.5)
    )
  ),
  "svmRadial" = list(
    tuneGrid = expand.grid(
      sigma = c(0.005, 0.01, 0.02),
      C = c(0.05, 0.1, 0.5)
    )
  ),
  "rf" = list(
    tuneGrid = expand.grid(
      mtry = 1:ncol(data)
    ),
    ntree = 500
  ),
  "glm" = list(
    weights = "dinamic"
  )
)

# ------------------------------------------------------------------------------
# Train model with imbalance ---------------------------------------------------
# ------------------------------------------------------------------------------
# models <- PEAXAI_fitting(
#   data = data,
#   x = x,
#   y = y,
#   z_numeric = z_numeric,
#   z_factor = z_factor,
#   RTS = RTS,
#   B = B,
#   m = m,
#   imbalance_rate = imbalance_rate,
#   methods = methods,
#   trControl = trControl,
#   metric_priority = metric_priority,
#   hold_out = hold_out,
#   seed = seed,
#   verbose = TRUE
# )
#
# save(models, file = "models_z.Rdata")
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/PEAXAI/models_z.Rdata")
# load("C:/Users/Ricardo/Documents/Doctorado EOMA/AAA Papers&Chapters/02_thesis_PEAXAI Z/Code/models_z.Rdata")

model <- "nnet"

# ------------------------------------------------------------------------------
# PEAXAI global importance features --------------------------------------------
# ------------------------------------------------------------------------------
# XAI method to determine relative importance of variables
importance_method <- list(
  name = "SHAP", # "SA", "SHAP", "PI", "LIME"
  bg_n = 200
  # method = "1D-SA", # only SA  c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  # measures = "AAD", # only SA  c("AAD", "gradient", "variance", "range")
  # levels = 7, # only SA
  # baseline = "mean" # only SA
  # n.repetitions = 50 # only PI
  # n_permutations = 5000, # only LIME
  # feature_select = "auto", # only LIME
  # bin_continuous = TRUE, # only LIME
  # n_bins = 4 # only LIME
)

devtools::load_all()
# relative_importance <- PEAXAI_global_importance(
#   final_model = models[["best_model_fit"]][[model]],
#   x = x,
#   y = y,
#   z_numeric = z_numeric,
#   z_factor = z_factor,
#   explain_data = data,
#   reference_data = models[["best_model_fit"]][[model]]$trainingData, # [,-1]
#   importance_method = importance_method,
#   seed = seed
# )
# save(relative_importance, file = "GLO_relative_importance_svmPoly.Rdata")

devtools::load_all()
# relative_importance_local <- PEAXAI_local_importance(
#   x = x,
#   y = y,
#   z_numeric = z_numeric,
#   z_factor = z_factor,
#   final_model = models[["best_model_fit"]][[model]],
#   explain_data = data,
#   reference_data = models[["best_model_fit"]][[model]]$trainingData,
#   importance_method = importance_method,
#   seed = seed
# )

# save(relative_importance_local, file = "LOCAL_relative_importance_svmPoly.Rdata")

# ------------------------------------------------------------------------------
# PEAXAI counterfactuals -------------------------------------------------------
# ------------------------------------------------------------------------------
# Efficiency thresholds
efficiency_thresholds <- seq(0.75, 0.95, 0.1) # seq(0.75, 0.95, 0.1)

relative_importance <- t(matrix(
  data = c(rep(0, 3), rep(1/3, 3)),
))
relative_importance <- as.data.frame(relative_importance)
names(relative_importance) <- names(data)[c(x,y)]

directional_vector <- list(
  relative_importance = relative_importance,
  baseline  = "mean"
)
devtools::load_all()

counterfactuals <- PEAXAI_counterfactuals(
  data = data,
  x = x,
  y = y,
  z_numeric = z_numeric,
  z_factor = z_factor,
  final_model = models[["best_model_fit"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  directional_vector = directional_vector,
  n_expand = 0.15,
  n_grid = 50,
  max_y = 1,
  min_x = 1
)

save(counterfactuals, file = "counterfactuals.Rdata")

colMeans(data[,5:7])
colMeans(counterfactuals$`0.75`$counterfactual_dataset[4:6])
colMeans(counterfactuals$`0.85`$counterfactual_dataset[4:6])
colMeans(counterfactuals$`0.95`$counterfactual_dataset[4:6])

print("Global vectors")
round((colMeans(counterfactuals$`0.75`$counterfactual_dataset[4:6]) - colMeans(data[,5:7])) / colMeans(data[,5:7]), 4)*100
mean(counterfactuals$`0.75`$inefficiencies$probability)

round((colMeans(counterfactuals$`0.85`$counterfactual_dataset[4:6]) - colMeans(data[,5:7])) / colMeans(data[,5:7]), 4)*100
mean(counterfactuals$`0.85`$inefficiencies$probability)

round((colMeans(counterfactuals$`0.95`$counterfactual_dataset[4:6]) - colMeans(data[,5:7])) / colMeans(data[,5:7]), 4)*100
mean(counterfactuals$`0.95`$inefficiencies$probability)

# ------------------------------------------------------------------------------
# PEAXAI ranking  --------------------------------------------------------------
# ------------------------------------------------------------------------------
ranking <- PEAXAI_ranking(
  data = data,
  x = x, y = y,
  final_model = models[["best_model_fit"]][[model]],
  rank_basis = "predicted"
)

ranking_plot <- as.data.frame(ranking["Probability_predicted"])
# rm(ranking_plot)

ranking <- PEAXAI_ranking(
  data = data,
  x = x,
  y = y,
  final_model = models[["best_model_fit"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  targets = counterfactuals,
  rank_basis = "attainable" # | "predicted" "attainable"
)
ranking_plot$`0.75` <- ranking$`0.75`$Probability_target
ranking_plot$`0.85` <- ranking$`0.85`$Probability_target
ranking_plot$`0.95` <- ranking$`0.95`$Probability_target
ranking_plot$position <- 1:nrow(ranking_plot)

library(ggplot2)
print(ggplot(data = ranking_plot, aes(x = position)) +

        geom_area(aes(y = `0.95`), fill = "black", alpha = 0.10) +
        geom_area(aes(y = `0.85`), fill = "black", alpha = 0.20) +
        geom_area(aes(y = `0.75`), fill = "black", alpha = 0.40) +


        geom_area(aes(y = Probability_predicted), fill = "black", alpha = 0.7) +

        geom_line(aes(y = Probability_predicted),
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
        ))

summary(data[, c(x,y)])
summary(counterfactuals$`0.75`$counterfactual_dataset)
# ----------------------------------------------------------------------------
# PEAXAI peer ----------------------------------------------------------------
# ----------------------------------------------------------------------------
peers <- PEAXAI_peer(
  data = data,
  x = x,
  y = y,
  final_model = models[["best_model_fit"]][[model]],
  efficiency_thresholds = efficiency_thresholds,
  targets = counterfactuals,
  weighted = FALSE,
  relative_importance = relative_importance
)

# }

