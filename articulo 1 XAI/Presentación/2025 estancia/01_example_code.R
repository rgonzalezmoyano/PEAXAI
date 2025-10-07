# ------------------------------------------------------------------------------
# EXAMPLE PEAXAI ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load package and libraries ---------------------------------------------------
# ------------------------------------------------------------------------------
devtools::load_all()
library(caret)
library(dplyr)
library(pROC)
library(PRROC)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# PEAXAI parameters ------------------------------------------------------------
# ------------------------------------------------------------------------------
# variables 
x <- 1
y <- 2
z <- NULL

# addresing imbalance
balance <- c(seq(0.1, 0.5, 0.1))

# efficiency thresholds
scenarios <- seq(0.60,0.95, 0.05)

# method of determine variable's importance
method_imp <- c("SHAP") # "SA", "SHAP", "PI"

# partition
hold_out <- 0.2 # https://topepo.github.io/caret/train-models-by-tag.html

# method 
method <- "nnet"

# control train
trControl <- trainControl(method = "none", classProbs = TRUE)

grid <- expand.grid(
  size = c(3),
  decay = c(0.01)) # NN

# ------------------------------------------------------------------------------
# Generate data ----------------------------------------------------------------
# ------------------------------------------------------------------------------
# set seed
seed <- 1997
set.seed(seed)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 200, # 200
    nX = 1
  )
)


# ------------------------------------------------------------------------------
# Save plots -------------------------------------------------------------------
# ------------------------------------------------------------------------------
width <- 30
height <- 20

save_plots <- FALSE

# plot theorical function
plot <- ggplot() +
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD), linewidth = 0.5) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "01_production_function.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}


# plot theorical function
plot <- ggplot() +
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD), linewidth = 0.5) +
  
  # observed data yD
  geom_point(data = data[72,], aes(x = x1, y = yD), size = 1) +
  
  # observed data y
  # geom_point(data = data[71,], aes(x = x1, y = y)) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "02_yD_example.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

# plot theorical function
plot <- ggplot() +
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD)) +
  
  # observed data yD
  geom_point(data = data[72,], aes(x = x1, y = yD), size = 1) +
  
  # observed data y
  geom_point(data = data[72,], aes(x = x1, y = y), size = 1) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot
if(save_plots == TRUE) {
  ggsave(
    filename = "03_y_example.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

# innefficieny
line_df <- data.frame(
  x    = data[72, 1],
  y    = data[72, 3],
  xend = data[72, 1],
  yend = data[72, 2]
)

plot <- ggplot() +
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD)) +
  
  # observed data yD
  geom_point(data = data[72,], aes(x = x1, y = yD), size = 1) +
  
  # observed data y
  geom_point(data = data[72,], aes(x = x1, y = y), size = 1) +
  
  # line
  geom_segment(
    data = line_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dashed",
    linewidth = 0.6,
    lineend = "round",
    arrow = arrow(ends = "last", type = "closed", length = unit(0.20, "cm"))) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "04_y_arrow_innefficiency.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

plot <- ggplot() +
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD)) +
  
  # observed data
  geom_point(data = data, aes(x = x1, y = y), size = 1) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "05_dataset_TF.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

plot <- ggplot() +
  
  # observed data
  geom_point(data = data, aes(x = x1, y = y), size = 1) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "06_dataset.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

# ------------------------------------------------------------------------------
# Step 1: Data labeling (validation first, train after) ------------------------
# ------------------------------------------------------------------------------

label_efficiency <- label_efficiency(
  data = data,
  x = x,
  y = y
)

x <- label_efficiency[["index"]][["x"]]
y <- label_efficiency[["index"]][["y"]]
z <- label_efficiency[["index"]][["z"]]

# plot DEA 
data_dea <- label_efficiency[["data_labeled"]]
idx_eff <- which(data_dea$class_efficiency == "efficient")
eff_dea <- data_dea[idx_eff, c(x,y)]
eff_dea <- eff_dea[order(eff_dea$x1),]
  
plot <- ggplot() +
  
  # theorical funtion
  # geom_line(data = data, aes(x = x1, y = yD), linetype = "dashed") +
  
  # observed data
  geom_point(
    data = data_dea,
    aes(x = x1, y = y),
    size = 1) +
  
  # # efficient data
  # geom_point(
  #   data = eff_dea,
  #   aes(x = x1, y = y),
  #   shape = 21,        # círculo con relleno
  #   fill  = "white",   # interior blanco
  #   color = "black",   # borde
  #   size  = 1.5,         # tamaño
  #   stroke = 0.6       # grosor del borde
  # ) +
  
  geom_line(
    data = eff_dea,
    aes(x = x1, y = y),
    size = 0.5) +

  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "07_dataset_DEA.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

p_zoom <- plot +
  
  # theorical funtion
  geom_line(data = data, aes(x = x1, y = yD), linetype = "dashed") +
  
  # efficient data
  geom_point(
    data = eff_dea,
    aes(x = x1, y = y),
    shape = 21,        # círculo con relleno
    fill  = "white",   # interior blanco
    color = "black",   # borde
    size  = 1.5,         # tamaño
    stroke = 0.6       # grosor del borde
  ) +
  coord_cartesian(xlim = c(3, 7), ylim = c(4,8), expand = FALSE) +
  # si quieres ver más “grueso” en el recorte:
  theme(text = element_text(size = 12))

p_zoom  # mostrar en pantalla
if(save_plots == TRUE) {
  ggsave(
    filename = "08_zoom_DEA.png",
    plot = p_zoom, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

plot <- ggplot() +
  
  # observed data
  geom_point(
    data = data_dea,
    aes(x = x1, y = y, colour = class_efficiency),
    size = 1.5) +
  
  # color
  scale_colour_manual(
    values = c(
      "not_efficient" = "#B64E4E",  # rojo oscuro pastel
      "efficient"     = "#4F8B61"   # verde oscuro pastel
    )) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom"); plot

if(save_plots == TRUE) {
  ggsave(
    filename = "09_dataset_label.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

# ------------------------------------------------------------------------------
# Step 2: Create validation set ------------------------------------------------
# ------------------------------------------------------------------------------
# reproduce index
set.seed(seed)

# create datasets
if (is.null(hold_out)) {
  # valid_index <- c(1:nrow(data))
  valid_data <- label_efficiency[["data_labeled"]]
  train_data <- label_efficiency[["data_labeled"]]
  
} else {
  
  # validation set
  valid_index <- createDataPartition(
    label_efficiency[["data_labeled"]]$class_efficiency,
    p = hold_out,
    list = FALSE)
  
  valid_data <- label_efficiency[["data_labeled"]][valid_index, ]
  train_data <- label_efficiency[["data_labeled"]][-valid_index,]
  all_data <- label_efficiency[["data_labeled"]]
  
  # # train and test set
  # # to check with validation
  # if (!is.null(test_out)) {
  #   copy_train_data <- train_data
  #   
  #   test_index <- createDataPartition(
  #     train_data$class_efficiency,
  #     p = test_out,
  #     list = FALSE)
  #   
  #   test_data <- train_data[test_index, ]
  #   train_data <- train_data[-test_index,]
  # }
  
  # # new label step
  # label_efficiency <- label_efficiency(
  #   data = train_data,
  #   x = x,
  #   y = y
  # )
  # 
  # train_data <- label_efficiency[["data_labeled"]]
  
}

prop.table(table(train_data$class_efficiency))
# prop.table(table(test_data$class_efficiency))
prop.table(table(valid_data$class_efficiency))
prop.table(table(all_data$class_efficiency))

plot <- ggplot() +
  
  # observed data
  geom_point(
    data = data_dea,
    aes(x = x1, y = y, colour = class_efficiency),
    size = 1.5,
    alpha = 0.2) +
  
  # train data
  geom_point(
    data = train_data,
    aes(x = x1, y = y, colour = class_efficiency),
    size = 1.5) +
  
  # color
  scale_colour_manual(
    values = c(
      "not_efficient" = "#B64E4E",  # rojo oscuro pastel
      "efficient"     = "#4F8B61"   # verde oscuro pastel
    )) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot

if(save_plots == TRUE) {
  ggsave(
    filename = "10_train_dataset.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

plot <- ggplot() +
  
  # observed data
  geom_point(
    data = data_dea,
    aes(x = x1, y = y, colour = class_efficiency),
    size = 1.5,
    alpha = 0.2) +
  
  # train data
  geom_point(
    data = valid_data,
    aes(x = x1, y = y, colour = class_efficiency),
    size = 1.5) +
  
  # color
  scale_colour_manual(
    values = c(
      "not_efficient" = "#B64E4E",  # rojo oscuro pastel
      "efficient"     = "#4F8B61"   # verde oscuro pastel
    )) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  
  scale_y_continuous(limits = c(0, 10)) +  
  
  # names labs
  labs(
    x = "x1",   
    y = "y"    
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom")
plot

if(save_plots == TRUE) {
  ggsave(
    filename = "11_valid_dataset.png",
    plot = plot, width = width, height = height, units = "cm",
    dpi = 600, device = "png"
  )
}

# ------------------------------------------------------------------------------
# Step 3: ML model training ----------------------------------------------------
# ------------------------------------------------------------------------------

  # ----------------------------------------------------------------------------
  # Step 3.1: Addressing imbalance rate ----------------------------------------
  # ----------------------------------------------------------------------------

train_data_SMOTE <- SMOTE_data(
  data = train_data,
  x = x,
  y = y,
  z = z,
  balance_data = balance
)

# add no balance scenario
balance <- c("original", balance)

train_data_SMOTE <- append(train_data_SMOTE, list(train_data), after = 0)
names(train_data_SMOTE)[1] <- "original"
names(train_data_SMOTE)

# save results structure dataframe
# columns
column_names <- c("imbalance", "size", "decay")

overall_names <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper",
                   "AccuracyNull", "AccuracyPValue", "McnemarPValue")

byClass_names <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
                   "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                   "Detection Prevalence", "Balanced Accuracy", "ROC_AUC", "PR_AUC",
                   "Best_threshold")

data_names <- names(data)[c(x,y)]

result_names <- c(column_names, overall_names, byClass_names, data_names) 

# Create dataframe
df_hyperparams <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams) <- result_names

df_hyperparams_train <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams_train) <- result_names

save_models <- list()

# train models and select the best
for(balance_i in balance) {
  
  print(balance_i)
  set.seed(seed)
  
  # train model
  model <- train(
    class_efficiency ~ .,
    data = train_data_SMOTE[[as.character(balance_i)]],
    method = method,
    trControl = trControl,
    tuneGrid = grid,
    metric = "Accuracy", 
    trace = FALSE
  )
  
  best_tuning <- grid
  
  # ----------------------------------------------------------------------------
  # Performance with TRAIN DATA ------------------------------------------------
  # ----------------------------------------------------------------------------
  train_data <- model[["trainingData"]][, 2:ncol(model[["trainingData"]])]
  y_obs <- model[["trainingData"]][[".outcome"]]
  train_data$class_efficiency <-  model[["trainingData"]][[".outcome"]]
  
  plot <- ggplot() +
    
    # observed data
    geom_point(
      data = train_data,
      aes(x = x1, y = y, colour = class_efficiency),
      size = 1.5,
      alpha = 1) +
    
    # color
    scale_colour_manual(
      values = c(
        "not_efficient" = "#B64E4E",  
        "efficient"     = "#4F8B61"
      )) +
    
    # exes
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(limits = c(0, 10)) +  
    scale_y_continuous(limits = c(0, 10)) +  
    
    # names labs
    labs(
      x = "x1",   
      y = "y"    
    ) +

    theme_bw() +
    theme(legend.position = "bottom")
  plot
  
  if (balance_i == "original") {
    
    name_i <- as.character(round(prop.table(table(valid_data$class_efficiency))[1], 3))
    
  } else {
    name_i <- balance_i
  }
  
  
  file_name <- paste0(name_i, "_train_set.png")
    
  if (save_plots == TRUE) {
    ggsave(
      filename = file_name,
      plot = plot, width = width, height = height, units = "cm",
      dpi = 600, device = "png"
    )
  }
  
  # ROC
  y_hat <- predict(model, newdata = train_data[,c(x,y)], type = "prob")[,1]
  # y_hat <- ifelse(y_hat == "not_efficient", 0, 1)
  y_obs <- ifelse(y_obs == "not_efficient", 0, 1)
  
  roc_obj <- roc(
    response = y_obs,
    predictor = y_hat,
    levels = c(0, 1),
    direction = "<")  
  
  auc(roc_obj)
  plot(roc_obj, print.thres = TRUE, print.auc = TRUE)
  
  threshold <- coords(roc_obj, "best",
                      ret = c("threshold", "sensitivity", "specificity", "accuracy"),
                      best.method = "youden")[1]
  
  threshold <- round(median(as.numeric(unlist(threshold))), 5)
  threshold_val <- threshold
  
  # PR AUC
  pr <- pr.curve(scores.class0 = y_hat[y_obs == 1],
                 scores.class1 = y_hat[y_obs == 0],
                 curve = TRUE)
  pr$auc.integral
  
  # Confusion matrix
  y_hat <- predict(model, newdata = train_data[,c(x,y)], type = "prob")[,1]
  y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
  y_hat <- factor(
    y_hat,
    levels = c("efficient", "not_efficient")
  )
  y_obs <- model[["trainingData"]][[".outcome"]]
  
  confusion_matrix <- caret::confusionMatrix(
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )
  
  df_hyperparams_train <- rbind(df_hyperparams_train, data.frame(
    imbalance = round(prop.table(table(train_data$class_efficiency))[1], 2),
    size  = best_tuning[1], # NN
    decay = best_tuning[2], # NN
    Accuracy = confusion_matrix$overall["Accuracy"],
    Kappa = confusion_matrix$overall["Kappa"],
    AccuracyLower = confusion_matrix$overall["AccuracyLower"],
    AccuracyUpper = confusion_matrix$overall["AccuracyUpper"],
    AccuracyNull = confusion_matrix$overall["AccuracyNull"],
    AccuracyPValue = confusion_matrix$overall["AccuracyPValue"],
    McnemarPValue = confusion_matrix$overall["McnemarPValue"],
    Sensitivity = confusion_matrix$byClass["Sensitivity"],
    Specificity = confusion_matrix$byClass["Specificity"],
    Pos_Pred_Value = confusion_matrix$byClass["Pos Pred Value"], 
    Neg_Pred_Value = confusion_matrix$byClass["Neg Pred Value"],
    Precision = confusion_matrix$byClass["Precision"],
    Recall = confusion_matrix$byClass["Recall"],
    F1 = confusion_matrix$byClass["F1"],
    Prevalence = confusion_matrix$byClass["Prevalence"],
    Detection_Rate = confusion_matrix$byClass["Detection Rate"],
    Detection_Prevalence = confusion_matrix$byClass["Detection Prevalence"],
    Balanced_Accuracy = confusion_matrix$byClass["Balanced Accuracy"],
    ROC_AUC = auc(roc_obj),
    PR_AUC = pr$auc.integral,
    Best_threshold = threshold
  ))
  
  # ----------------------------------------------------------------------------
  # Performance with VALIDATION DATA -------------------------------------------
  # ----------------------------------------------------------------------------
  # ROC AUC
  y_hat <- predict(model, newdata = valid_data[,c(x,y)], type = "prob")[,1]
  y_obs <- valid_data$class_efficiency
  y_obs <- ifelse(y_obs == "not_efficient", 0, 1)
  
  roc_obj <- roc(
    response = y_obs,
    predictor = y_hat,
    levels = c(0, 1),
    direction = "<")
  
  auc(roc_obj)
  plot(roc_obj, print.thres = TRUE, print.auc = TRUE)
  
  # PR AUC
  pr <- pr.curve(scores.class0 = y_hat[y_obs == 1],
                 scores.class1 = y_hat[y_obs == 0],
                 curve = TRUE)
  pr$auc.integral
  
  threshold <- coords(roc_obj, "best",
                      ret = c("threshold", "sensitivity", "specificity", "accuracy"),
                      best.method = "youden")[1]
  
  threshold <- round(as.numeric(threshold), 5)
  
  # confusion matrix
  y_hat <- predict(model, newdata = valid_data[,c(x,y)], type = "prob")[,1]
  y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
  y_hat <- factor(
    y_hat,
    levels = c("efficient", "not_efficient")
  )
  y_obs <- valid_data$class_efficiency
  
  confusion_matrix <- caret::confusionMatrix(
    data = y_hat,
    reference = y_obs,
    mode = "everything",
    positive = "efficient"
  )
  
  # importance variables
  # Asegura que la clase positiva es la PRIMERA (importa para type="prob")
  valid_data$class_efficiency <- factor(valid_data$class_efficiency,
                                        levels = c("efficient","not_efficient"))
  
  # Matriz de características (sin la respuesta)
  X <- valid_data[, setdiff(names(valid_data), "class_efficiency"), drop = FALSE]
  
  # Función de predicción que devuelve P(clase positiva)
  f_pred <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")[, "efficient"]
  }
  
  # SHAP
  set.seed(seed)
  shap_model <- fastshap::explain(
    object       = model,   
    X            = X,       
    pred_wrapper = f_pred,
    newdata      = X,       
    nsim         = 2048      
  )
  
  # global importance
  imp <- data.frame(
    feature    = colnames(shap_model),
    importance = colMeans(abs(shap_model), na.rm = TRUE)
  )

  
  # normalize importances
  imp_norm <- imp
  imp_norm$importance_norm <- imp_norm$importance / sum(imp_norm$importance)
  importance <- imp_norm$importance_norm
  importance
  
  df_hyperparams <- rbind(df_hyperparams, data.frame(
    imbalance = round(prop.table(table(train_data$class_efficiency))[1], 2),
    size  = best_tuning[1], # NN
    decay = best_tuning[2], # NN
    Accuracy = confusion_matrix$overall["Accuracy"],
    Kappa = confusion_matrix$overall["Kappa"],
    AccuracyLower = confusion_matrix$overall["AccuracyLower"],
    AccuracyUpper = confusion_matrix$overall["AccuracyUpper"],
    AccuracyNull = confusion_matrix$overall["AccuracyNull"],
    AccuracyPValue = confusion_matrix$overall["AccuracyPValue"],
    McnemarPValue = confusion_matrix$overall["McnemarPValue"],
    Sensitivity = confusion_matrix$byClass["Sensitivity"],
    Specificity = confusion_matrix$byClass["Specificity"],
    Pos_Pred_Value = confusion_matrix$byClass["Pos Pred Value"], 
    Neg_Pred_Value = confusion_matrix$byClass["Neg Pred Value"],
    Precision = confusion_matrix$byClass["Precision"],
    Recall = confusion_matrix$byClass["Recall"],
    F1 = confusion_matrix$byClass["F1"],
    Prevalence = confusion_matrix$byClass["Prevalence"],
    Detection_Rate = confusion_matrix$byClass["Detection Rate"],
    Detection_Prevalence = confusion_matrix$byClass["Detection Prevalence"],
    Balanced_Accuracy = confusion_matrix$byClass["Balanced Accuracy"],
    ROC_AUC = auc(roc_obj),
    PR_AUC = pr$auc.integral,
    Best_threshold = threshold,
    x1 = importance[1],
    y = importance[2]
  ))
  
  # # plot
  # # make a grid of the predictors
  # grid_pred <- expand.grid (
  #   x1 = seq(0, 10, length = 600),
  #   y = seq(0, 10, length = 600)
  # )
  # 
  # grid_pred$class_efficiency <- predict(model, newdata = grid_pred, type = "prob")[,1]
  # 
  # red <- "#B64E4E"
  # green <- "#4F8B61"
  # 
  # plot <- ggplot() +
  #   geom_raster(
  #     data = grid_pred,
  #     aes(x = x1, y = y, fill = class_efficiency),
  #     alpha = 0.5
  #   ) +
  #   geom_point(
  #     data = data_dea,
  #     aes(x = x1, y = y, colour = class_efficiency),
  #     size = 1.2, alpha = 0.15
  #   ) +
  #   geom_point(
  #     data = valid_data,
  #     aes(x = x1, y = y, colour = class_efficiency),
  #     size = 1.4
  #   ) +
  #   
  #   scale_fill_gradientn(
  #     colors  = c(red,  "white",  "white",  "white", green),
  #     values  = rescale(c(0.00, 0.25,    0.50,     0.75,         1.00)),
  #     limits  = c(0, 1),
  #     oob     = squish,
  #     guide   = "none",
  #     name    = "Prob."
  #   ) +
  #   
  #   geom_contour(
  #     data = grid_pred,
  #     aes(x = x1, y = y, z = class_efficiency),
  #     breaks = 0.50,            # nivel deseado
  #     colour = "black",         # o "black"
  #     linewidth = 0.25,
  #     linetype = "solid",
  #     show.legend = FALSE,
  #     alpha = 0.3
  #   ) +
  #   
  #   # color
  #   scale_colour_manual(
  #     values = c(
  #       "not_efficient" = red,  
  #       "efficient"     = green   
  #     )) +
  #   
  #   # names labs
  #   labs(
  #     x = "x1",   
  #     y = "y"    
  #   ) +
  #   
  #   # exes
  #   geom_hline(yintercept = 0) +
  #   geom_vline(xintercept = 0) +
  #   scale_x_continuous(limits = c(0, 10)) +  
  #   scale_y_continuous(limits = c(0, 10)) +  
  #   
  #   # names labs
  #   labs(
  #     x = "x1",   
  #     y = "y"    
  #   ) +
  #   
  #   theme_bw() +
  #   theme(legend.position = "bottom")
  # plot
  #   # coord_equal() 
  # 
  # if (balance_i == "original") {
  #   balance_i <- as.character(round(prop.table(table(valid_data$class_efficiency))[1], 3))
  # }
  # name_i <- paste0(balance_i, "_grid.png")
  # 
  # if(save_plots == TRUE) {
  #   ggsave(
  #     filename = name_i,
  #     plot = plot, width = width, height = height, units = "cm",
  #     dpi = 600, device = "png"
  #   )
  # }
  # 
  # plot
}

df_hyperparams_train
df_hyperparams

df_hyperparams$imbalance <- as.numeric(df_hyperparams$imbalance)

# order 
select_balance <- df_hyperparams %>%
  arrange(desc(Balanced_Accuracy), desc(Precision), desc(Sensitivity), imbalance)

final_grid <- select_balance[1,2:3] # NN

# train final model
if (select_balance[1,1] != round(prop.table(table(valid_data$class_efficiency))[1], 2)) {
  
  best_balance <- as.numeric(select_balance[1,1])
  
  # final_model <- save_models[[as.character(tied_imbalance)]]
  
  # balance to best rate
  train_data <- SMOTE_data(
    data = all_data,
    x = x,
    y = y,
    balance = best_balance
  )[[as.character(best_balance)]]
  
  # train the model
  final_model <- train(
    class_efficiency ~.,
    data = train_data,
    method = method,
    trControl = trainControl(method = "none", classProbs = TRUE),
    tuneGrid = best_tuning,
    metric = "Accuracy",
    trace = FALSE
  )
  
} else {
  
  # train the model
  final_model <- train(
    class_efficiency ~ .,
    data = all_data,
    method = method,
    trControl = trainControl(method = "none", classProbs = TRUE),
    tuneGrid = best_tuning,
    metric = "Accuracy",
    maxit = 200,
    trace = FALSE
  )
  
}

if (length(x) == 1) {
  
  # plot of final model
  # make a grid of the predictors
  grid_pred <- expand.grid (
    x1 = seq(0, 10, length = 600),
    y = seq(0, 10, length = 600)
  )
  
  grid_pred$class_efficiency <- predict(final_model, newdata = grid_pred, type = "prob")[,1]
  
  red <- "#B64E4E"
  green <- "#4F8B61"
  red_lt   <- "#F2DADA"
  green_lt <- "#DCEFE3"
  
  plot <- ggplot() +
    geom_raster(
      data = grid_pred,
      aes(x = x1, y = y, fill = class_efficiency),
      alpha = 0.5
    ) +
    # geom_point(
    #   data = data_dea,
    #   aes(x = x1, y = y, colour = class_efficiency),
    #   size = 1.2, alpha = 0.15
    # ) +
    geom_point(
      data = all_data,
      aes(x = x1, y = y, colour = class_efficiency),
      size = 1.4
    ) +
    geom_contour(
      data = grid_pred,
      aes(x = x1, y = y, z = class_efficiency),
      breaks = 0.8,            # nivel deseado
      colour = "black",         # o "black"
      linewidth = 0.5,
      linetype = "solid",
      show.legend = FALSE,
      alpha = 0.4
    ) +
    
    # DEA
    geom_line(
      data = eff_dea,
      aes(x = x1, y = y),
      size = 0.5, linetype = "longdash",
      alpha = 0.6
    ) +
    
    # God
    geom_line(data = data, aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
    
    scale_fill_gradientn(
      colors  = c(red,  red_lt,  "white",  green_lt, green),
      values  = rescale(c(0.00, 0.25,    0.50,     0.75,         1.00)),
      limits  = c(0, 1),
      oob     = squish,
      guide   = "none",
      name    = "Prob."
    ) +
    
    # color
    scale_colour_manual(
      values = c(
        "not_efficient" = red,  
        "efficient"     = green   
      )) +
    
    # names labs
    labs(
      x = "x1",   
      y = "y"    
    ) +
    
    # exes
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(limits = c(0, 10)) +  
    scale_y_continuous(limits = c(0, 10)) +  
    
    # names labs
    labs(
      x = "x1",   
      y = "y"    
    ) +
    
    theme_bw() +
    theme(legend.position = "bottom")
  plot
  
  if (save_plots == TRUE) {
    ggsave(
      filename = "12_final_model.png",
      plot = plot, width = width, height = height, units = "cm",
      dpi = 600, device = "png"
    )
  }
  
}


# ------------------------------------------------------------------------------
# Variable importance detection ------------------------------------------------
# ------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # Understanding the decision model -------------------------------------------
  # ----------------------------------------------------------------------------

train_data <- final_model[["trainingData"]]
names(train_data)[1] <- "class_efficiency"
train_data <- train_data %>%
  relocate(class_efficiency, .after = last_col())

if (method_imp == "SA") {
  # ========================= #
  # Sensitivity Analisys (SA) #
  # ========================= #
  
  if(!(is.null(z))) {
    
    dataset_dummy <- dummy_cols(train_data,  select_columns = c(names(train_data))[z+1]) %>%
      select(-c(names(train_data))[z+1])
    
    train_data <- dataset_dummy
    
    
    to_factor <- c((x+y+1):ncol(train_data))
    train_data <- change_class(train_data, to_factor = to_factor)
    
  } else {
    
    train_data <- train_data[,c(2:length(train_data),1)]
    
  }
  
  # Define methods and measures
  methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
  
  levels <- 7
  set.seed(0)
  # with rminer
  
  if (method == "nnet") {
    m <- rminer::fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model[["bestTune"]][["size"]],
      decay = final_model[["bestTune"]][["decay"]] #final_model$decay
      #entropy = FALSE
      #softmax = TRUE
    )
  } else if (method == "ksvm") {
    m <- rminer::fit(
      ClassEfficiency ~ .,
      data  = train_data,
      model = "ksvm",
      scale = "none",
      C      = final_model[["bestTune"]][["C"]],
      degree = final_model[["bestTune"]][["degree"]],
      scale.par = final_model[["bestTune"]][["scale"]]  # a veces llamado "scale" en caret
    )
  } else if (method == "lm") {
    
  }
  
  # Calculate the importance for the current method and measure
  importance <- Importance(
    M = m,
    RealL = levels, # Levels
    data = train_data,
    method = methods_SA,
    measure = measures_SA,
    baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
    responses = TRUE
    
  )
  
  importance <- round(importance$imp, 4)
  importance
  
} else if (method_imp == "SHAP") {
  # ==== #
  # SHAP #
  # ==== #
  # Asegura que la clase positiva es la PRIMERA (importa para type="prob")
  train_data$class_efficiency <- factor(train_data$class_efficiency,
                                        levels = c("efficient","not_efficient"))
  
  # Matriz de características (sin la respuesta)
  X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
  
  # Función de predicción que devuelve P(clase positiva)
  f_pred <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")[, "efficient"]
  }
  
  # SHAP para todas las filas usando X como background (nsim controla precisión/tiempo)
  set.seed(0)
  shap_model <- fastshap::explain(
    object       = final_model,   
    X            = X,       
    pred_wrapper = f_pred,
    newdata      = X,       
    nsim         = 2048      
  )
  
  # Importancia global = media del |SHAP| por variable
  imp <- data.frame(
    feature    = colnames(shap_model),
    importance = colMeans(abs(shap_model), na.rm = TRUE)
  )
  # imp <- imp[order(-imp$importance), ]
  # imp
  
  # Normalizar importancias y añadir % (0–100)
  imp_norm <- imp[order(-imp$importance), , drop = FALSE]
  imp_norm$importance_norm <- imp_norm$importance / sum(imp_norm$importance, na.rm = TRUE)
  importance <- imp_norm$importance_norm
  importance
  
} else if (method_imp == "PI") {
  # =========================== #
  # Permutation Importance (PI) #
  # =========================== #
  # Asegura que la clase positiva es la PRIMERA (importa para type="prob")
  train_data$class_efficiency <- factor(train_data$class_efficiency,
                                        levels = c("efficient","not_efficient"))
  
  # Matriz de características (sin la respuesta)
  X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
  
  # 3) Wrapper de predicción: devuelve P(clase positiva)
  #    (robusto al nombre de columna por si cambiaste niveles después de entrenar)
  f_pred <- function(object, newdata) {
    pr <- as.data.frame(predict(object, newdata = newdata, type = "prob"))
    pos_col <- if ("efficient" %in% names(pr)) "efficient" else names(pr)[1]
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
  
  # 5) Función de pérdida basada en AUC (menor = peor)
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

importance_model <- importance

# ----------------------------------------------------------------------------
# Understanding the real data ------------------------------------------------
# ----------------------------------------------------------------------------
train_data <- all_data
if (method_imp == "SA") {
  # ========================= #
  # Sensitivity Analisys (SA) #
  # ========================= #
  
  if(!(is.null(z))) {
    
    dataset_dummy <- dummy_cols(train_data,  select_columns = c(names(train_data))[z+1]) %>%
      select(-c(names(train_data))[z+1])
    
    train_data <- dataset_dummy
    
    
    to_factor <- c((x+y+1):ncol(train_data))
    train_data <- change_class(train_data, to_factor = to_factor)
    
  } else {
    
    train_data <- train_data[,c(2:length(train_data),1)]
    
  }
  
  # Define methods and measures
  methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
  measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
  
  levels <- 7
  set.seed(0)
  # with rminer
  
  if (method == "nnet") {
    m <- rminer::fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model[["bestTune"]][["size"]],
      decay = final_model[["bestTune"]][["decay"]] #final_model$decay
      #entropy = FALSE
      #softmax = TRUE
    )
  } else if (method == "ksvm") {
    m <- rminer::fit(
      ClassEfficiency ~ .,
      data  = train_data,
      model = "ksvm",
      scale = "none",
      C      = final_model[["bestTune"]][["C"]],
      degree = final_model[["bestTune"]][["degree"]],
      scale.par = final_model[["bestTune"]][["scale"]]  # a veces llamado "scale" en caret
    )
  } else if (method == "lm") {
    
  }
  
  # Calculate the importance for the current method and measure
  importance <- Importance(
    M = m,
    RealL = levels, # Levels
    data = train_data,
    method = methods_SA,
    measure = measures_SA,
    baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
    responses = TRUE
    
  )
  
  importance <- round(importance$imp, 4)
  importance
  
} else if (method_imp == "SHAP") {
  # ==== #
  # SHAP #
  # ==== #
  # Asegura que la clase positiva es la PRIMERA (importa para type="prob")
  train_data$class_efficiency <- factor(train_data$class_efficiency,
                                        levels = c("efficient","not_efficient"))
  
  # Matriz de características (sin la respuesta)
  X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
  
  # Función de predicción que devuelve P(clase positiva)
  f_pred <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")[, "efficient"]
  }
  
  # SHAP para todas las filas usando X como background (nsim controla precisión/tiempo)
  set.seed(0)
  shap_model <- fastshap::explain(
    object       = final_model,   
    X            = X,       
    pred_wrapper = f_pred,
    newdata      = X,       
    nsim         = 2048      
  )
  
  # Importancia global = media del |SHAP| por variable
  imp <- data.frame(
    feature    = colnames(shap_model),
    importance = colMeans(abs(shap_model), na.rm = TRUE)
  )
  # imp <- imp[order(-imp$importance), ]
  # imp
  
  # Normalizar importancias y añadir % (0–100)
  imp_norm <- imp[order(-imp$importance), , drop = FALSE]
  imp_norm$importance_norm <- imp_norm$importance / sum(imp_norm$importance, na.rm = TRUE)
  importance <- imp_norm$importance_norm
  importance
  
} else if (method_imp == "PI") {
  # =========================== #
  # Permutation Importance (PI) #
  # =========================== #
  # Asegura que la clase positiva es la PRIMERA (importa para type="prob")
  train_data$class_efficiency <- factor(train_data$class_efficiency,
                                        levels = c("efficient","not_efficient"))
  
  # Matriz de características (sin la respuesta)
  X <- train_data[, setdiff(names(train_data), "class_efficiency"), drop = FALSE]
  
  # 3) Wrapper de predicción: devuelve P(clase positiva)
  #    (robusto al nombre de columna por si cambiaste niveles después de entrenar)
  f_pred <- function(object, newdata) {
    pr <- as.data.frame(predict(object, newdata = newdata, type = "prob"))
    pos_col <- if ("efficient" %in% names(pr)) "efficient" else names(pr)[1]
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
  
  # 5) Función de pérdida basada en AUC (menor = peor)
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

importance_data <- importance


# # 1 vector
# importance <- rep(1, length(data[,c(new_x,new_y)]))

# ------------------------------------------------------------------------------
# Get ranking ------------------------------------------------------------------
# ------------------------------------------------------------------------------
data_rank <- all_data[, c(x,y)]

eff_vector <- apply(data_rank, 1, function(row) {
  
  row_df <- as.data.frame(t(row))
  
  colnames(row_df) <- names(data_rank)
  
  pred <- unlist(predict(final_model, row_df, type = "prob")[1])
  
  return(pred)
})

eff_vector <- as.data.frame(round(eff_vector, 4))

id <- as.data.frame(c(1:nrow(data)))
names(id) <- "id"
eff_vector <- cbind(id, eff_vector)
names(eff_vector)[2] <- "eff_vector"

ranking_order <- eff_vector[order(eff_vector$eff_vector, decreasing = TRUE), ]
plot(ranking_order$eff_vector[1:200])
df <- ranking_order
df$position <- 1:nrow(ranking_order)

ggplot() +
  geom_line(data = as.data.frame(df), aes(x = position, y = eff_vector)) +
  theme_minimal()

# ============================= #
# to get probabilities senarios #
# ============================= #
importance <- importance_model
importance <- c(0, 1)
data_save <- all_data[,c(x,y)]
result_SA <- data.frame(
  t(importance) 
)
names(result_SA) <- names(all_data)[c(x, y)]
data_scenario_list <- list()
metrics_list <- list()
peer_list <- list()
peer_weight_list <- list()
na_count_list <- list()
n_not_prob_list <- list()

devtools::load_all()
for (e in 1:length(scenarios)) {
  print(paste("scenario: ", scenarios[e]))
  print(final_model)
  
  data_scenario <- compute_target(
    data = data_save[,c(x,y)],
    x = x,
    y = y,
    z = z,
    final_model = final_model,
    cut_off = scenarios[e],
    imp_vector = result_SA
  )
  
  if(all(is.na(data_scenario$data_scenario))) {
    print("all na")
    browser()
    
    # peer
    peer_restult <- NA
    
    # save_peer
    peer_list[[e]] <- peer_restult
    
    # main_metrics
    main_metrics <- NA
    
    # save main_metrics
    metrics_list[[e]] <- main_metrics
    
    print("pause")
    
  } else {
    
    if(any(data_scenario$data_scenario[, c(x,y)] < 0)) {
      
      data_scenario$data_scenario[apply(data_scenario$data_scenario, 1, function(row) any(row < 0) || any(is.na(row))), ] <- NA
      
      na_idx <- which(apply(data_scenario$data_scenario, 1, function(row) any(is.na(row))))
      data_scenario$betas[na_idx,] <- NA
    }
    
    data_scenario_list[[e]] <- data_scenario
    
    # ================ #
    # determinate peer #
    # ================ #
    
    # first, determinate efficient units
    idx_eff <- which(eff_vector$eff_vector > scenarios[e])
    
    if (!length(idx_eff) == 0) {
      
      # save distances structure
      save_dist <- matrix(
        data = NA,
        ncol = length(idx_eff),
        nrow = nrow(data_save)
      )
      
      # save weighted distances structure
      save_dist_weight <- matrix(
        data = NA,
        ncol = length(idx_eff),
        nrow = nrow(data_save)
      )
      
      # calculate distances
      for (unit_eff in idx_eff) {
        
        # set reference
        reference <- data_save[unit_eff, c(x,y)]
        
        distance <- unname(apply(data_save[, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
        
        # get position in save results
        idx_dis <- which(idx_eff == unit_eff)
        
        save_dist[,idx_dis] <- as.matrix(distance)
      }
      
      near_idx_eff <- apply(save_dist, 1, function(row) {
        
        which.min(abs(row))
        
      })
      
      peer_restult <- matrix(
        data = NA,
        ncol = 1,
        nrow = nrow(data_save)
      )
      
      peer_restult[, 1] <- idx_eff[near_idx_eff]
      
      # save_peer
      peer_list[[e]] <- peer_restult
      
      # calculate weighted distances
      result_SA_matrix <- as.data.frame(matrix(
        data = rep(unlist(result_SA[c(x,y)]), each = nrow(data_save)),
        nrow = nrow(data_save),
        ncol = ncol(data_save[,c(x,y)]),
        byrow = FALSE
      ))
      names(result_SA_matrix) <- names(data_save)[c(x,y)]
      
      w_eval_data <- data_save[, c(x, y)] * result_SA_matrix
      
      for (unit_eff in idx_eff) {
        
        # set reference
        reference <- data_save[unit_eff, c(x,y)]
        
        distance <- unname(apply(data_save[, c(x, y)], 1, function(row) {
          sqrt((sum(result_SA[c(x,y)] * ((row - reference)^2))))
        }))
        
        # get position in save results
        idx_dis <- which(idx_eff == unit_eff)
        save_dist_weight[,idx_dis] <- as.matrix(distance)
      }
      
      near_idx_eff_weight <- apply(save_dist_weight, 1, function(row) {
        
        which.min(abs(row))
        
      })
      
      peer_restult_weight <- matrix(
        data = NA,
        ncol = 1,
        nrow = nrow(save_dist_weight)
      )
      
      peer_restult_weight[, 1] <- idx_eff[near_idx_eff]
      
      # save_peer
      peer_weight_list[[e]] <- peer_restult_weight
      
      # join data plus betas to metrics for scenario
      data_metrics <- cbind(data_scenario$data_scenario, round(data_scenario$betas, 5))
      
      # number not scenario
      n_not_prob <- which(data_metrics$probability < scenarios[e])
      n_not_prob_list[[e]] <- n_not_prob
      
      # count na
      na_row <- which(apply(data_metrics, 1, function(row) all(is.na(row))))
      count_na <- length(na_row)
      na_count_list[[e]] <- count_na
      
      # metrics: mean, median, sd
      main_metrics <- as.data.frame(matrix(
        data = NA,
        ncol = ncol(data_metrics),
        nrow = 3
      ))
      
      # metrics
      main_metrics[1,] <- apply(data_metrics, 2, mean, na.rm = TRUE)
      main_metrics[2,] <- apply(data_metrics, 2, median, na.rm = TRUE)
      main_metrics[3,] <- apply(data_metrics, 2, sd, na.rm = TRUE)
      
      names(main_metrics) <- names(data_metrics)
      row.names(main_metrics) <- c("mean", "median", "sd")
      
      metrics_list[[e]] <- main_metrics
      
    } else {
      
      peer_list[[e]] <- NULL
      # na_count_list[[e]] <- nrow(eval_data)
      metrics_list[[e]] <- NULL
    }
    
  }
  
} # end loop scenarios

# ------------------------------------------------------------------------------
# Results simulation -----------------------------------------------------------
# ------------------------------------------------------------------------------
dea_bcc <- dea_out <- dea(
  X = data[,x], Y = data[,y],
  RTS = "vrs", ORIENTATION = "out")

data_score <- data.frame(
  "y_D" = data$yD/data$y,
  "y_DEA-BCC" = dea_bcc$eff,
  "y_PEAXAI-0.60" = data_scenario_list[[1]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.65" = data_scenario_list[[2]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.70" = data_scenario_list[[3]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.75" = data_scenario_list[[4]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.80" = data_scenario_list[[5]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.85" = data_scenario_list[[6]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.90" = data_scenario_list[[7]][["data_scenario"]][["y"]]/data$y,
  "y_PEAXAI-0.95" = data_scenario_list[[8]][["data_scenario"]][["y"]]/data$y
  
)

# funtion to analisys
metrics_vs_yD <- function(truth, est) {
  
  # filtra NA/Inf de forma coherente en ambas variables
  idx <- is.finite(truth) & is.finite(est)
  y  <- truth[idx]
  p  <- est[idx]
  n  <- length(y)
  stopifnot(n > 0)
  
  mse  <- mean((y - p)^2)
  bias <- mean(y - p)      
  mae <- mean(abs(y - p)) 
  
  # correlaciones
  r_pearson  <- suppressWarnings(cor(p, y, method = "pearson"))
  r_spearman <- suppressWarnings(cor(p, y, method = "spearman"))
  r_kendall  <- suppressWarnings(cor(p, y, method = "kendall"))
  
  data.frame(
    MSE = mse,
    Bias = bias,
    MAE = mae,
    Pearson = r_pearson,
    Spearman = r_spearman,
    Kendall = r_kendall
  )
}


# aplica a todas las columnas estimadas
calc_all_metrics <- function(data_score) {
  stopifnot("y_D" %in% names(data_score))
  y <- data_score[["y_D"]]
  
  est_cols <- setdiff(names(data_score), "y_D")
  out_list <- lapply(
    est_cols,
    function(col) {
      
    metrics_vs_yD(y, data_score[[col]])
      
      }
    )
  
  out <- do.call(rbind, out_list)
  rownames(out) <- est_cols
  out

}

# ejemplo de uso:
results <- calc_all_metrics(data_score)
print(round(results, 4))

