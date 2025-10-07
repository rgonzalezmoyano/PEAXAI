#########################################
# Generate results for NNET evaluations #
#########################################

# ------------------------------------------------------------------------------
# Load libraries and package ---------------------------------------------------
# ------------------------------------------------------------------------------
# Package PEAXAI
devtools::load_all()

# Visualize data
library(ggplot2)
library(scales)

# ML
library(caret)

# ------------------------------------------------------------------------------
# Load datasets ----------------------------------------------------------------
# ------------------------------------------------------------------------------
load("articulo 1 XAI/code paper/Comparation diferent models/save_datasets_comparation.Rdata")

# ------------------------------------------------------------------------------
# Save betas hyperparameters ---------------------------------------------------
# ------------------------------------------------------------------------------
# TECHNIQUE
technique <- "NNET"

# SCENARIO
scenario <- c(1:8)

# CUT OFF
cut_off <- c(0.55, 0.65, 0.75, 0.85, 0.95)

# number of real units
n <- nrow(save_data[[2]])

# number of bootstrap trainning
m <- 10

# structure
df <- data.frame(
  matrix(NA_real_, nrow = n, ncol = length(cut_off)),
  check.names = FALSE             # <- mantiene "0.75" sin convertir a "X0.75"
)
colnames(df) <- as.character(cut_off)

for (scenario_i in scenario) {
  
  set.seed(1)
  # ------------------------------------------------------------------------------
  # SVM  -------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  # Get the labeled dataset
  data_original <- save_data[[2]]
  
  # changing dataset by imbalance rate
  if (scenario_i == 1) {
    data <- save_data[[2]] # original dataset
  } else if (scenario_i == 2) {
    data <- save_data[[3]][["0.2"]]
  } else if (scenario_i == 3) {
    data <- save_data[[3]][["0.25"]]
  } else if (scenario_i == 4) {
    data <- save_data[[3]][["0.3"]]
  } else if (scenario_i == 5) {
    data <- save_data[[3]][["0.35"]]
  } else if (scenario_i == 6) {
    data <- save_data[[3]][["0.4"]]
  } else if (scenario_i == 7) {
    data <- save_data[[3]][["0.45"]]
  } else if (scenario_i == 8) {
    data <- save_data[[3]][["0.5"]]
  } else {
    stop("scenario debe ser un entero entre 1 y 8.")
  }
  
  # determine inputs/outputs
  x <- 1
  y <- 2
  yD <- 3
  
  # determine direction
  direction <- c(0,1)
  
  # # Get TRUE dataset for predictions
  # data_yD <- save_data[[1]][,c(x,yD)]
  # names(data_yD) <- names(data)[c(x,y)]
  
  # fit model
  # metrics for model evaluation
  MySummary <- function (data, lev = NULL, model = NULL) {
    
    # accuracy and kappa
    acc_kpp <- defaultSummary(data, lev, model)
    
    # AUC, sensitivity and specificity
    auc_sen_spe <- twoClassSummary(data, lev, model)
    
    # precision and recall
    pre_rec <- prSummary(data, lev, model)
    
    c(acc_kpp, auc_sen_spe, pre_rec)
    
  } 
  
  trControl <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = MySummary,
    classProbs = TRUE,
    savePredictions = "all"
  )

  grid <- expand.grid(
    size  = c(1, 5, 10, 20, 30),                # nº de neuronas ocultas
    decay = c(0.1, 0.01, 0.001, 0.001)          # regularización L2
  )

  model <- train(
    class_efficiency ~ .,
    data = data,
    method = "nnet",
    trControl = trControl,
    preProcess = c("center","scale"),
    tuneGrid = grid,
    metric = "Accuracy",
    trace = FALSE,
    maxit = 500,
    MaxNWts = 10000
  )
  
  # prediction
  pred <- predict(
    model,
    newdata = data[, c(x,y)],
    type = "prob")[1]
  
  # ------------------------------------------------------------------------------
  # Draw model's probabilities ---------------------------------------------------
  # ------------------------------------------------------------------------------
  length_n <- 500
  gx <- seq(0, 10, length.out = length_n)
  gy <- seq(0, 10, length.out = length_n)
  
  grid_xy <- expand.grid(
    x1 = gx,
    y  = gy
  )
  pred <- predict(
    model,
    newdata = grid_xy[, c(x,y)],
    type = "prob")
  
  grid_xy$pred <- as.matrix(pred[,1])
  
  # --- 1) Asegura que 'pred' es la columna correcta (prob. de la clase positiva) ---
  vars <- names(data)[c(x, y)]
  
  # pred_grid <- predict(model, newdata = grid_xy[, vars, drop = FALSE], type = "prob")
  # pos_class <- if ("efficient" %in% names(pred_grid)) "efficient" else levels(data$class_efficiency)[1]
  # grid_xy$pred <- as.numeric(pred_grid[[pos_class]])
  
  # --- 2) Define los niveles (0.1, 0.2, ..., 0.9) ---
  brks <- seq(0.75, 0.95, by = 0.1)
  # brks <- c(0.25, 0.5, 0.75, 0.90, 0.95)
  ggplot() +
    # Fondo de probabilidades
    geom_raster(
      data = grid_xy,
      aes(x = x1, y = y, fill = pred),
      interpolate = FALSE,
      alpha = 0.6
    ) +
    scale_fill_gradient2(
      low = "#F59AA3", mid = "white", high = "#8FD7A5",
      midpoint = 0.5, limits = c(0, 1), oob = squish,
      name = "P(Efficiency)"
    ) +
    
    # Curvas de nivel 0.1, 0.2, ..., 0.9
    geom_contour(
      data = grid_xy,
      aes(x = x1, y = y, z = pred),
      breaks = brks,
      colour = "grey30",
      linewidth = 0.4
    ) +
    
    # # Resalta la isoprobabilidad 0.5 (frontera de decisión)
    # geom_contour(
    #   data = grid_xy,
    #   aes(x = x1, y = y, z = pred),
    #   breaks = 0.5,
    #   colour = "black",
    #   linewidth = 0.8
    # ) +
    
    # (opcional) puntos y línea de referencia
    geom_point(
      data = data_original, aes(x = x1, y = y),
      color = "black", size = 1, inherit.aes = FALSE
    ) +
    geom_line(
      data = save_data[[1]], aes(x = x1, y = yD),
      color = "black", alpha = 0.5, linewidth = 1, linetype = "dashed",
      inherit.aes = FALSE
    ) +
    
    coord_cartesian(xlim = range(grid_xy$x1), ylim = range(grid_xy$y), expand = FALSE) +
    theme_minimal() +
    theme(legend.position = "bottom")
 
  # ------------------------------------------------------------------------------
  # Calculate betas values -------------------------------------------------------
  # ------------------------------------------------------------------------------
  for (cut_off_i in cut_off) {
    betas <- compute_target(
      data = data_original[,c(x,y)],
      x = x,
      y = y,
      final_model = model,
      cut_off = cut_off_i,
      imp_vector = direction
    )[["betas"]][,1]
    
    # save betas
    df[,as.character(cut_off_i)] <- betas
  }
  
  # ------------------------------------------------------------------------------
  # Save df by scenario ----------------------------------------------------------
  # ------------------------------------------------------------------------------
  if (scenario_i == 1) {
    scenario_name <- "original"
  } else if (scenario_i == 2){
    scenario_name <- "0.2"
  } else if (scenario_i == 3) {
    scenario_name <- "0.25"
  } else if (scenario_i == 4) {
    scenario_name <- "0.3"
  } else if (scenario_i == 5) {
    scenario_name <- "0.35"
  } else if (scenario_i == 6) {
    scenario_name <- "0.4"
  } else if (scenario_i == 7) {
    scenario_name <- "0.45"
  } else if (scenario_i == 8) {
    scenario_name <- "0.5"
  } else {
    stop("scenario_i debe ser un entero entre 1 y 8.")
  }
   
  name_df <- paste0(technique, "_", scenario_name, ".Rdata")
  rute <- "articulo 1 XAI/code paper/Comparation diferent models/"
  
  name_df <- paste(rute, name_df)
  save(df, file = name_df)
  
}

