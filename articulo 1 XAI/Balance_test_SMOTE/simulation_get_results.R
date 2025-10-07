# get metric ML and importance

# ============================= #
# valencian comunity  companies #
# ============================= #

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
library(openxlsx)


# ===
# load data
# ===

# ======================= #
# Valencian Comunity 2018 #
# ======================= #

load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
#load("C:/Users/Ricardo/Documents/Doctorado EOMA/Cafee/articulo 1 XAI/data_valencia_comunity/firms.RData")
data <- firms

# save a copy
data_original <- data

# make changes realted to class 
data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
data <- data[data$autonomous_community == "Comunidad Valenciana",]

# ===
# Information to cafee
# ===

# x and y indexes
x <- c(9:12)
y <- c(8)

# different types to label
target_method <- "additive"

seed <- 0

print(seed)
set.seed(seed)

methods <- list (
  # neuronal network
  "nnet" = list(
    hyparams = list(
      "size" = c(1, 5, 10, 15, 20, 30),
      "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
    ),
    options = list (
      maxit = 1000,
      softmax = TRUE
    )
  )
)

# =========== #
# score cafee #
# =========== #    
# scenarios to peer
scenarios <- seq(0.75, 0.95, 0.1)

# efficiency orientation
prop_loop <- c(0, 0.2, 0.3, 0.4, 0.5)
prop_inef_loop <- c(0, 4, 3, 2)
grid <- expand.grid(prop_inef_loop = prop_inef_loop, prop_loop = prop_loop)
grid <- grid[-c(2:4),]


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

hold_out <- 0.00
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE
returns <- "variable"

# save model information
list_method <- list()  

results <- as.data.frame(matrix(
  data = NA,
  ncol = 22,
  nrow = nrow(grid)
))

results[,1] <- grid$prop_loop
results[,2] <- grid$prop_inef_loop

for (row in 1:nrow(results)) {
  print(row/nrow(results)* 100)
  balance_data <- c(grid$prop_loop[row], grid$prop_inef_loop[row])
  
  # bucle methods
  for (i in 1:length(methods)) {
    
    # console information
    print(paste("METODO:", i,  names(methods)[i]))
    print("")
    
    # model result
    final_model <- efficiency_estimation (
      data = data,
      x = x,
      y = y,
      #z = z,
      balance_data = balance_data,
      trControl = trControl,
      method = methods[i],
      target_method = target_method,
      metric = metric,
      hold_out = hold_out,
      scenarios = scenarios
    )
  
    # save results
    results[row, 3] <- nrow(final_model$final_model$trainingData)
    results[row, 4:14] <- final_model$selected_model_metrics
    results[row, 15:19] <- final_model$result_SA
    results[row, 20:22] <- t(as.matrix(final_model$count_na))
print(results)
  } # end bucle for (methods)  
  
} # end loop grid

names(results) <- c("eff_%", "make ineff unit each", "number dataset",
                    names(final_model$selected_model_metrics), names(final_model$result_SA),
                    names(final_model$count_na))

# write.xlsx(results, file = "results_NN.xlsx")
# write.xlsx(result_SA, file = "result_NN.xlsx")
