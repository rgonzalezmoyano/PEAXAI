#New code paper1 valencian example
# ------------------------------------------------------------------------------
# Load package and libraries ---------------------------------------------------
# ------------------------------------------------------------------------------
devtools::load_all()
library(caret)
library(dplyr)
library(pROC)
library(PRROC)
# library(Benchmarking)
# library(magrittr)
# 
# # library(deaR)
# library(haven)
# # library(e1071)
# library(rminer)
# # library(fastDummies)
# # library(keras)
# library(MLmetrics)
# library(fastshap)
# library(shapviz)
# library(iml)
# library(pROC)
# library(rminer)

# ------------------------------------------------------------------------------
# Load data --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ======================== #
# Valencian Community 2018 #
# ======================== #
load("articulo 1 XAI/data_valencia_comunity/firms.RData")
data <- firms

# save a copy
data_original <- data

# # make changes realted to class
# data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
# data <- data[data$autonomous_community == "Comunidad Valenciana",]

# filter varaibles
data <- data[, c(8:12)]

# eliminate df from environment
rm(data_original)
rm(firms)

# ------------------------------------------------------------------------------
# PEAXAI parameters ------------------------------------------------------------
# ------------------------------------------------------------------------------
# variables 
x <- 2:5
y <- 1
z <- NULL

# addresing imbalance
balance <- c(seq(0.1, 0.5, 0.1))

# efficiency thresholds
scenarios <- seq(0.75,0.95, 0.1)

# method ML
method <- "nnet"

# control train
trControl <- trainControl(method = "none", classProbs = TRUE)

# method of determine variable's importance
method_imp <- c("SHAP") # "SA", "SHAP", "PI"

grid <- expand.grid(
  size = c(5),
  decay = c(0.001)) # NN

# set seed 
seed <- 0

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

# ------------------------------------------------------------------------------
# Create validation set --------------------------------------------------------
# ------------------------------------------------------------------------------
hold_out <- 0.4 # https://topepo.github.io/caret/train-models-by-tag.html

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
  
  # train and test set
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

# ------------------------------------------------------------------------------
# Step 2: Addressing imbalance rate --------------------------------------------
# ------------------------------------------------------------------------------
train_data_SMOTE <- SMOTE_data(
  data = train_data,
  x = x,
  y = y,
  z = z,
  balance_data = balance
)

# train_data_val <- SMOTE_data(
#   data = copy_train_data,
#   x = x,
#   y = y,
#   balance_data = balance
# )


balance <- c("original", balance)


# add no balance scenario
train_data_SMOTE <- append(train_data_SMOTE, list(train_data), after = 0)
names(train_data_SMOTE)[1] <- "original"
names(train_data_SMOTE)

# copy_train_data <- train_data
# copy_train_data_SMOTE <- train_data_SMOTE
# copy_valid_data <- valid_data
# save_data <- list(copy_train_data, copy_train_data_SMOTE, copy_valid_data, label_efficiency, valid_index)
# save(save_data, file = "save_data_valencian_2.Rdata")

# ------------------------------------------------------------------------------
# Step 3: ML model training ----------------------------------------------------
# ------------------------------------------------------------------------------
# save results structure dataframe
# columns
column_names <- c("imbalance", "size", "decay")

overall_names <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper",
                   "AccuracyNull", "AccuracyPValue", "McnemarPValue")

byClass_names <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
                   "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                   "Detection Prevalence", "Balanced Accuracy", "ROC_AUC", "PR_AUC",
                   "Best_threshold")

result_names <- c(column_names, overall_names, byClass_names) 

# Create dataframe
df_hyperparams <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams) <- result_names

df_hyperparams_train <- data.frame(matrix(ncol = length(result_names), nrow = 0))
colnames(df_hyperparams_train) <- result_names

# grid
# grid <- expand.grid(
#   size = c(1, 2, 3, 4, 5),
#   decay = c(0.1, 0.01, 0.001))

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
  
  # # Confusion matrix using VALIDATION dataset
  # y_hat <- predict(
  #   model,
  #   newdata = valid_data[,c(x,y)],
  #   type = "prob")[,1]
  # 
  # y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
  # y_hat <- factor(
  #   y_hat,
  #   levels = c("efficient", "not_efficient")
  # )
  # y_obs <- model[["trainingData"]][[".outcome"]]
  # 
  # confusion_matrix <- caret::confusionMatrix(
  #   data = y_hat,
  #   reference = y_obs,
  #   mode = "everything",
  #   positive = "efficient"
  # )
  # 
  # model[["results"]] <- c(confusion_matrix[["overall"]], confusion_matrix[["byClass"]])
  # 
  # # ----------------------------------
  # # IMPLEMENTAR VALIDACIÓN CRUZADA ---
  # # ----------------------------------
  # 
  # # best grid
  # best_tuning <- grid 
  # 
  # # apply balance
  # if (balance_i != "original") {
  #   train_data_val_i <- train_data_val[[as.character(balance_i)]][,c(x,y)]
  # } else {
  #   train_data_val_i <- copy_train_data[,c(x,y)]
  # }
  # 
  # # train the best model to compare performance in train and validation dataset
  # best_model <- train(
  #   class_efficiency ~ .,
  #   data = train_data_val_i,
  #   method = method,
  #   trControl = trainControl(method = "none", classProbs = TRUE),
  #   tuneGrid = best_tuning,
  #   metric = "Accuracy",
  #   maxit = 200,
  #   trace = FALSE
  # )
  # 
  # save_models[[as.character(balance_i)]] <- best_model
  
  # ----------------------------------------------------------------------------
  # Perfornance with TRAIN DATA ------------------------------------------------
  # ----------------------------------------------------------------------------
  train_data <- model[["trainingData"]][, 2:ncol(model[["trainingData"]])]
  y_obs <- model[["trainingData"]][[".outcome"]]
  
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
    imbalance = balance_i,
    size  = best_tuning[1], # NN
    decay = best_tuning[2], # NN
    # degree = best_tuning[1], # SVM
    # scale = best_tuning[2], # SVM
    # C = best_tuning[3], # SVM
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
  # Perfornance with VALIDATION DATA -------------------------------------------
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
 
  df_hyperparams <- rbind(df_hyperparams, data.frame(
    imbalance = balance_i,
    size  = best_tuning[1], # NN
    decay = best_tuning[2], # NN
    # degree = best_tuning[1], # SVM
    # scale = best_tuning[2], # SVM
    # C = best_tuning[3], # SVM
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
  
}

df_hyperparams_train
df_hyperparams

validation_performance <- df_hyperparams
train_performance <- df_hyperparams_train

# sort
df_hyperparams <- df_hyperparams %>%
  arrange(desc(F1), desc(Balanced_Accuracy), desc(Precision), desc(Sensitivity))

# check if there are tied
tied_imbalance <- df_hyperparams %>%
  filter(dplyr::near(F1, max(F1))) %>%   # empates en la métrica principal
  pull(imbalance) %>%
  unique() %>%
  sort()

# tie?
if (!(length(tied_imbalance) == 1)) {
  
  if (!is.null(hold_out)) {
    
    if (!"original" %in% tied_imbalance) {
      
      tied_imbalance <- "original"
      
    } else {
      
      tied_imbalance <- min(as.numeric(tied_imbalance))
      
    }
    
  } else {
    
    balance <- balance[which(balance %in% tied_imbalance)]
    
    # new dataframe results
    df_hyperparams <- data.frame(matrix(ncol = length(result_names), nrow = 0))
    colnames(df_hyperparams) <- result_names
    
    for (balance_i in balance) {
      print(balance_i)
      
      y_hat <- predict(save_models[[as.character(balance_i)]], train_data_SMOTE[[as.character(balance_i)]])
      y_obs <- train_data_SMOTE[[as.character(balance_i)]]$class_efficiency
      
      confusion_matrix <- caret::confusionMatrix(
        data = y_hat,
        reference = y_obs,
        mode = "everything",
        positive = "efficient"
      )
      
      df_hyperparams <- rbind(df_hyperparams, data.frame(
        imbalance = balance_i,
        size  = best_tuning[1], # NN
        decay = best_tuning[2], # NN
        # degree = best_tuning[1], # SVM
        # scale = best_tuning[2], # SVM
        # C = best_tuning[3], # SVM
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
        Balanced_Accuracy = confusion_matrix$byClass["Balanced Accuracy"]
      ))
      
    }
    
    df_hyperparams
    train_decision_balance <- df_hyperparams
    
    # sort
    df_hyperparams <- df_hyperparams %>%
      arrange(desc(F1), desc(Balanced_Accuracy), desc(Precision), desc(Sensitivity))
    
    View(df_hyperparams)
    
    # check if there are tied
    tied_imbalance <- df_hyperparams %>%
      filter(dplyr::near(Accuracy, max(Accuracy))) %>%   # empates en la métrica principal
      pull(imbalance) %>%
      unique() %>%
      sort() 
    
    if ("original" %in% tied_imbalance) {
      tied_imbalance <- "original"
    } else {
      tied_imbalance <- tied_imbalance[1]
    }
    
  }
  
}

final_grid <- df_hyperparams[1,2:3] # NN
# final_grid <- df_hyperparams[1,2:4] # SVM

# join train + validation set
if (df_hyperparams[1,1] != "original") {
  
  best_balance <- as.numeric(df_hyperparams[1,1])
  
  # final_model <- save_models[[as.character(tied_imbalance)]]
  
  # balance to best rate
  train_data <- SMOTE_data(
    data = all_data,
    x = x,
    y = y,
    balance = best_balance
  )[[as.character(best_balance)]]
  
  str(train_data)
  
  # train the model
  final_model <- train(
    class_efficiency ~.,
    data = train_data,
    method = "nnet",
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
    method = "nnet",
    trControl = trainControl(method = "none", classProbs = TRUE),
    tuneGrid = best_tuning,
    metric = "Accuracy",
    maxit = 200,
    trace = FALSE
  )
  
}

# join train + validation set
# final_data <- rbind(train_data, valid_data)
# 
# # train best model with all data
# final_model <- train(
#   class_efficiency ~.,
#   data = final_data,
#   method = "nnet",
#   trControl = trainControl(method = "none", classProbs = TRUE),
#   tuneGrid = final_grid,
#   metric = "Accuracy",
#   maxit = 200,
#   trace = FALSE
# )

# # # NN paper
# load("articulo 1 XAI/code paper/resultados_art_XAI_NN_CV_1_3.RData")
# final_model <- list_method[["nnet"]][["final_model"]]
# # final_model <- final_model$finalModel
# tied_imbalance <- list_method[["nnet"]][["best_balance"]]
# # # #
# importance <- list_method[["nnet"]][["result_SA"]]


# # re
# y_hat <- predict(final_model, newdata = all_data[,c(x,y)], type = "prob")[,1]
# y_hat <- ifelse(y_hat > 0.5, "efficient", "not_efficient")
# y_hat <- factor(
#   y_hat,
#   levels = c("efficient", "not_efficient")
# )
# y_obs <- all_data$class_efficiency
# 
# confusion_matrix <- caret::confusionMatrix(
#   data = y_hat,
#   reference = y_obs,
#   mode = "everything",
#   positive = "efficient"
# )[["byClass"]]
# performance_real_data <- confusion_matrix

# ------------------------------------------------------------------------------
# Variable importance detection ------------------------------------------------
# ------------------------------------------------------------------------------

# data <- label_efficiency[["data_labeled"]]

# necesary data to calculate importance in rminer
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
plot(ranking_order$eff_vector[1:50])
# ============================= #
# to get probabilities senarios #
# ============================= #
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
      na_count_list[[e]] <- nrow(eval_data)
      metrics_list[[e]] <- NULL
    }
    
  }
  
} # end loop scenarios

# # create confusion matrix and calculate metrics related to confusion matrix
# performance_real_data <- caret::confusionMatrix(
#   data = y_hat,
#   reference = y_obs,
#   mode = "everything",
#   positive = "efficient"
# )[["byClass"]]

final_model <- list(
  validation_performance = validation_performance,
  train_performance = train_performance,
  best_balance = best_balance,
  final_model = final_model,
  # performance_train_dataset = selected_model,
  # performance_real_data = performance_real_data,
  importance = importance,
  result_SA = result_SA,
  eff_vector = eff_vector,
  ranking_order = ranking_order,
  peer_list = peer_list,
  peer_weight_list = peer_weight_list,
  data_scenario_list = data_scenario_list,
  metrics_list = metrics_list,
  count_na = na_count_list,
  n_not_prob_list = n_not_prob_list
)

# save(final_model, file = "result_NN_g_x0_y0.Rdata")
# save(final_model, file = "result_SVM_g_mean.Rdata")
# save(data_scenario_list, file = "betas_NN.Rdata")
