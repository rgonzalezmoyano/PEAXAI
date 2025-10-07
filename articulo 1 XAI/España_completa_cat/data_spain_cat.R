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

library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(haven)

# ===
# load data
# ===
data_2018 <- read_dta("C:/Users/Ricardo/Downloads/Data Spain PISA 2018.dta")
data_2018$Region <- as.factor(data_2018$Region)
data_2018$SCHLTYPE <- as.factor(data_2018$SCHLTYPE)


# ===
# Table
# ===

# x and y indexes
x <- c(3:5)
y <- c(10, 7, 6, 2, 8)

# different types to label
target_method <- "additive"

set.seed(314)
  
list_information <- list()

# ======= #
# svmPoly #
# ======= #
methods <- list (
  # svm
  "svmPoly" = list(
    "degree" = c(1, 2, 3, 4, 5),
    "scale" = c(0.001, 0.1, 1, 10, 100),
    "C" = c(0.001, 0.1, 1, 10, 100)
  ),
  "svmRadial" = list(
    "sigma" = c(0.01, 0.1, 1, 10, 100),
    "C" = c(0.001, 0.1, 1, 10, 100)
  ),
  
  # random forest
  "rf" = list (
    mtry = c(1, 2)
  ),
  
  # CART
  "rpart" = list (
    cp = c(0.001, 0.01, 0.1, 0.2, 0.3)
  ),
  
  # neuronal network
  "nnet" = list(
    "size" = c(1, 5, 10, 20),  # Número de nodos en la capa oculta
    "decay" = c(0, 0.1, 0.01, 0.001)  # Tasa de decaimiento del peso
  )
)

scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data_2018)
)

scores <- as.data.frame(scores)
    
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
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)
    
hold_out <- 0.10
    
# https://topepo.github.io/caret/train-models-by-tag.html
    
metric = "F1"

data <- data_2018  

for (i in 1:length(methods)) {
  print(i)
  # Result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    orientation = orientation,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = "F1",
    hold_out = hold_out
  )
  
  # ======================== #
  # bset cut off is selected #
  # ======================== #
  scores_cafee <- compute_scores (
    data = data,
    x = x,
    y = y,
    final_model = final_model,
    orientation = orientation,
    cut_off = final_model[["cut_off"]]
  )        
  
  # information
  list <- list()
  
  list[[1]] <- final_model$method
  list[[2]] <- final_model$bestTune
  
  list_information[[i]] <- list
  
  scores[[i]] <- scores_cafee 
}

names(scores) <- names(methods)
     
        
       
 
  
  # ====== #
  # server #
  # ====== #
  
  file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
  save(simulaciones, file = file)
  
  file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
  save(list_information, file = file_information)