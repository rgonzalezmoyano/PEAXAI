source("/home/PI/ricardo.gonzalezm/cafee/R/balanced_data.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/compute_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_estimation.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/efficiency_scores.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/preprocessing.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/projection.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/simulations.R")
source("/home/PI/ricardo.gonzalezm/cafee/R/training.R")

# ========== #
# Spain 2018 #
# ========== #

# ===
# libraries
# ===
devtools::load_all()
library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(haven)
library(e1071)
library(rminer)

# ===
# load data
# ===

#############
# PISA 2018 #
#############
data_2018 <- read_dta("C:/Users/Ricardo/Downloads/Data Spain PISA 2018.dta")
data_2018$Region <- as.factor(data_2018$Region)
data_2018$SCHLTYPE <- as.factor(data_2018$SCHLTYPE)

# preProces
data_NA <- data_2018[which(is.na(data_2018$SCHLTYPE)), ]
borrar <- table(data_NA$Region)

inf_NA <- matrix(
  data = NA,
  ncol = 3,
  nrow = length(unique(data_2018$Region))
)

inf_NA <- as.data.frame(inf_NA)

names(inf_NA) <- c("region", "num_NA", "percent_NA")

idx_reg <- sort(unique(data_2018$Region))

inf_NA$region <- idx_reg

for (i in 1:nrow(inf_NA)) {
  
  i_data <- data_2018 %>% 
    filter(Region == i)
  
  value <- sum(apply(i_data, 1, anyNA))
  
  inf_NA$num_NA[i] <- value
  inf_NA$percent_NA[i] <- round(value / nrow(i_data) * 100, 2)
  
}

# save errors and NA in models
inf_NA

# ===
# Information to cafee
# ===

# x and y indexes
x <- c(10, 7, 6)
y <- c(3:5)
#z <- c(2, 8) # environment variables


# different types to label
target_method <- "BCC"

set.seed(314)
methods <- list (
  "svmPoly" = list(
    hyparams = list(
      "degree" = c(3),
      "scale" = c(0.1),
      "C" = c(1)
    )
  )
  
  # # svm
  # "svmPoly" = list(
  #     hyparams = list(
  #       "degree" = c(1, 2, 3, 4, 5),
  #       "scale" = c(0.001, 0.1, 1, 10, 100),
  #       "C" = c(0.001, 0.1, 1, 10, 100)
  #     )
  # ),
  # "svmRadial" = list(
  #   hyparams = list(
  #     "sigma" = c(0.01, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # ),

  # # random forest
  # "rf" = list (
  #   options = list (
  #     ntree = c(500) # c(100, 500, 1000)
  #   ),
  #   hyparams = list(
  #     mtry = c(4)
  #   )
  # ),
#
#   # neuronal network
#   "nnet" = list(
#     hyparams = list(
#       "size" = c(1, 5, 10, 20),
#       "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
#       ),
#     options = list (
#       maxit = 1000
#     )
#   )
  
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
orientation <- "output"

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

# Parameters for controlling the training process
trControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.10
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE
returns <- "variable"

# preProcess
data <- data_2018
data <- data[1:200, ]
idx_NA <- which(is.na(data$SCHLTYPE))
data <- data[-idx_NA,]

# save scores region
list_region <- list()

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data)
) 
  
# change to data.frame
scores <- as.data.frame(scores)

# change names
score_names <- names(methods)
names(scores) <- score_names

# save model information
list_method <- list()  

# bucle region
for (i in 1:length(methods)) {
  
  # console information
  print(paste("METODO:", i))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
    orientation = orientation,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    convexity = convexity,
    returns = returns
  )
  
  #final_model <- information_region[[2]][[2]][[1]]
  
  # bset cut off is selected 
  scores_cafee <- compute_scores (
    data = data,
    x = x,
    y = y,
    #z = z,
    final_model = final_model$final_model,
    orientation = orientation,
    cut_off = final_model$final_model[["cut_off"]]
  )  
  
  scores[i] <- scores_cafee
  
  # # Importance of variables
  # # varImp Caret
  # importance <- varImp(object = final_model$final_model)
  # print(importance)
  # 
  # plot <- plot(importance)
  
  if (names(methods[i]) == "rf") {
  
    data_oob <- as.data.frame(final_model[["final_model"]][["finalModel"]][["err.rate"]])
    ntrees <- c(1:final_model$final_model$dots$ntree)

    data_oob_plot <- cbind(ntrees, data_oob)

    ggplot(data = data_oob_plot) +
      geom_line(aes(x = ntrees, y = OOB))
  
    # importance by r miner
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    # con rminer pero no escala
    m_rf <- fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "randomForest",
      scale = "none",
      mtry = methods$rf$hyparams$mtry,
      ntree = methods$rf$options$ntree
    )
  
    rf.imp <- Importance(m_rf, data = train_data)
    imp_value <- rf.imp$imp
  
    importance <- matrix(
      data = NA,
      ncol = 2,
      nrow = length(names(train_data))
    )
  
    importance <- as.data.frame(importance)
  
    importance$V1 <- names(train_data)
    importance$V2 <- imp_value
  
    names(importance) <- c("", "Overall")
  
    importance <- importance[order(-importance$Overall), ]
  
    importance
  
    # comprobar m_rf
    scores_cafee_m_rf <- compute_scores (
      data = data,
      x = x,
      y = y,
      z = z,
      final_model = m_rf,
      orientation = orientation,
      cut_off = final_model[["final_model"]][["cut_off"]]
    )  
  
  }
  
  if (names(methods[i]) == "svmPoly") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"

    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    # con rminer pero no escala
    m <- fit(
      ClassEfficiency~.,
      data = train_data,
      model = "ksvm",
      kernel = "polydot",
      scale = "none",
      kpar = list(
        degree = final_model$final_model$bestTune$degree,
        scale = final_model$final_model$bestTune$scale
      ),
      C = final_model$final_model$bestTune$C
    )

    # Define methods and measures
    methods_SA <- c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD", "gradient", "variance", "range")
    
    # make grid SA
    grid_SA <- expand.grid(method = methods_SA, measure = measures_SA)
    
    # save results
    results_SA <- data.frame(method = character(), measure = character())
    
    # Loop through each combination of method and measure
    for (a in 1:nrow(grid_SA)) {
      
      method <- as.character(grid_SA$method[a])
      measure <- as.character(grid_SA$measure[a])
      
      # Calculate the importance for the current method and measure
      importance <- Importance(m, data = train_data, method = method, measure = measure)
      
      # Extract the importance values (assuming 26 values)
      imp_values <- importance$imp
      
      # Create a row with method, measure, and the 26 importance values
      result_row <- data.frame(method = method, measure = measure, t(imp_values))
      
      # Append the row to results_SA
      results_SA <- rbind(results_SA, result_row)
    }
    
    names(results_SA)[3:ncol(results_SA)] <- names(train_data)
  
  } else if (names(methods[i]) == "svmRadial") {
      
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    # con rminer pero no escala
    m_rad <- fit(
      ClassEfficiency~.,
      data = train_data,
      model = "ksvm",
      kernel = "rbfdot",
      scale = "none",
      C = final_model$bestTune$C,
      kpar = list(sigma = final_model$bestTune$sigma)
    )
    
    svm.imp <- Importance(m_rad, data = train_data)
    imp_value <- svm.imp$imp
    
    importance <- matrix(
      data = NA,
      ncol = 2,
      nrow = length(names(train_data))
    )
    
    importance <- as.data.frame(importance)
    
    importance$V1 <- names(train_data)
    importance$V2 <- imp_value
    
    names(importance) <- c("", "Overall")
    
    importance <- importance[order(-importance$Overall), ]
    
  } else if (names(methods[i]) == "nnet") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    # con rminer
    m <- fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model$final_model$bestTune$size,
      decay = final_model$final_model$bestTune$decay
    )
    
    # Define methods and measures
    methods_SA <- c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD", "gradient", "variance", "range")
    
    # make grid SA
    grid_SA <- expand.grid(method = methods_SA, measure = measures_SA)
    
    # save results
    results_SA <- data.frame(method = character(), measure = character())
    
    # Loop through each combination of method and measure
    for (a in 1:nrow(grid_SA)) {
      
      method <- as.character(grid_SA$method[a])
      measure <- as.character(grid_SA$measure[a])
      
      # Calculate the importance for the current method and measure
      importance <- Importance(m, data = train_data, method = method, measure = measure)
      
      # Extract the importance values (assuming 26 values)
      imp_values <- importance$imp
      
      # Create a row with method, measure, and the 26 importance values
      result_row <- data.frame(method = method, measure = measure, t(imp_values))
      
      # Append the row to results_SA
      results_SA <- rbind(results_SA, result_row)
    }
    
    names(results_SA)[3:ncol(results_SA)] <- names(train_data)
    
    }

  
  # information model
  list <- list()
  
  list[[1]] <- final_model
  list[[2]] <- results_SA
  list[[3]] <- m
  list[[4]] <- importance

  
  list_method[[i]] <- list
  
} # end bucle for (methods)  
  
information_region <- list()
information_region[[1]] <- scores
information_region[[2]] <- list_method


names(information_region) <- c("scores","ML_models")

# names final object
names(information_region) <- c("scores","ML_models")
  
names(information_region[["ML_models"]]) <- names(methods)
  
names(information_region[["ML_models"]][1]) <- "model_fit"
names(information_region[["ML_models"]][2]) <- "metrics_model"

for(i in 1:length(methods)) {
  names(information_region[["ML_models"]][[names(methods[i])]]) <- c("model", "importance")
}

save(information_region, file = "resultados_art_XAI_def.RData")


library(Benchmarking)

scores_sdea <- sdea (
  X = as.matrix(data[, y]),
  Y = as.matrix(data[, x]),
  RTS = "vrs",
  ORIENTATION = "out"
)$eff; scores_sdea
min(scores_sdea)
scores_sdea <- round(scores_sdea, 3)

scores <- as.data.frame(cbind(information_region[[1]][["svmPoly"]], information_region[[1]][["nnet"]]))
names(scores) <- c("svmPoly", "nnet")
  
# XAI paper
result_final <- as.data.frame(
  matrix(
    data = NA,
    ncol = 5,
    nrow = nrow(data)
  )
)

scores_sdea <- ifelse(scores_sdea == "-Inf", NA, scores_sdea)
scores_final <- cbind(scores_sdea, scores)

omit <- which(is.na(scores_final$svmPoly))

round(cor(x = information_region[[1]][["svmPoly"]][-omit], y = information_region[[1]][["nnet"]][-omit], method = "spearman"), 3)
round(cor(x = DEA_score, y = information_region[[1]][["nnet"]], method = "spearman"), 3)
round(cor(x = DEA_score[-omit], y = information_region[[1]][["svmPoly"]][-omit], method = "spearman"), 3)

DEA_score <- rad_out (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = TRUE,
  returns = "variable"
) 

entrenamiento_data <- final_model[["final_model"]][["trainingData"]]
proporcion <- prop.table(table(entrenamiento_data$.outcome)) 

save.image("Information_R_PISA.RData")

# ====== #
# server #
# ====== #

file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
save(simulaciones, file = file)

file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
save(list_information, file = file_information)