################################################################################
######################### EXAMPLE OF USE PEAXAI ################################
################################################################################

# ------------------------------------------------------------------------------
# Load PEAXAI ------------------------------------------------------------------
# ------------------------------------------------------------------------------
devtools::load_all()
usethis::create_package("C:/Users/Ricardo/Documents/PEAXAI")
# ------------------------------------------------------------------------------
# Load dataset -----------------------------------------------------------------
# ------------------------------------------------------------------------------
load("data/firms.RData")


# save a copy
data_original <- data

# make changes realted to class
data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
data <- data[data$autonomous_community == "Comunidad Valenciana",]
# data$current_assets <- data$total_assets - data$fixed_assets
# data <- cbind(data[,1:8], data[,13], data[,10:12])
# names(data)[9] <-  "current_assets"
# ===
# Information to cafee
# ===

# x and y indexes
x <- c(9:12)
y <- c(8)
#z <- c(2, 8) # environment variables

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
      "decay" = c(0.1, 0.01, 0.001, 0,0001)
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

# SMOTE proportions
balance_data <- c(0, seq(0.20, 0.4, 0.05)) # c(0, seq(0.20, 0.4, 0.05))

# ML metric
metric = "F"

# scenarios to peer
scenarios <- seq(0.75, 0.95, 0.1) # seq(0.75, 0.95, 0.1)

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

# parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.20 # https://topepo.github.io/caret/train-models-by-tag.html

# save model information
list_method <- list()

set.seed(314)

# loop method
for (i in 1:length(methods)) {

  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")

  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    # z = z,
    balance_data = balance_data,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    scenarios = scenarios
  )

  list_method[[i]] <- final_model

} # end bucle for (methods)

names(list_method) <- names(methods)

#save(list_method, file = "resultados_art_XAI_NN_CV_1_3.RData")
#
library(openxlsx)

# write.xlsx(list_method$nnet$metrics, file = "metrics_NN.xlsx")
# write.xlsx(list_method$svmPoly$metrics, file = "metrics_SVM.xlsx")
#write.xlsx(summary(data[, c(9:12, 8)]), file = "summary.xlsx")

list_method[["nnet"]][["final_model"]][["trainingData"]][1:97,]
#
plot(density(list_method[["nnet"]][["ranking_order"]][["eff_vector"]]),
     main = "Gráfico de Densidad Probability",
     xlab = "Valores",
     ylab = "Densidad",
     col = "blue",
     lwd = 2)
hist(list_method[["nnet"]][["ranking_order"]][["eff_vector"]],
     probability = TRUE,
     col = rgb(0, 0, 1, 0.3),
     border = "white",
     add = TRUE)

plot(density(list_method[["nnet"]][["data_scenario_list"]][["0.75"]][["betas"]][["beta"]]),
     main = "Gráfico de Densidad Betas",
     xlab = "Valores",
     ylab = "Densidad",
     col = "blue",
     lwd = 2)
hist(list_method[["nnet"]][["data_scenario_list"]][["0.75"]][["betas"]][["beta"]],
     probability = TRUE,
     col = rgb(0, 0, 1, 0.3),
     border = "white",
     add = TRUE)

data_complete_NN <- cbind(data[, c(x,y)], list_method[["nnet"]][["data_contrafactual"]])
# data_complete_SVM <- cbind(data[, c(x,y)], list_method[["svmPoly"]][["data_contrafactual"]])
#
# write.xlsx(data_complete_NN, file = "data_complete_NN.xlsx")
# write.xlsx(data_complete_SVM, file = "data_complete_SVM.xlsx")
#
# write.xlsx(list_method[["svmPoly"]][["resume_metrics"]], file = "statistics_metrics_SVM.xlsx")
# write.xlsx(list_method[["nnet"]][["resume_metrics"]], file = "statistics_metrics_NN.xlsx")

write.xlsx(list_method[["nnet"]][["real_decision_balance"]], file = "real_decision_balance.xlsx")
write.xlsx(list_method[["nnet"]][["train_decision_balance"]], file = "train_decision_balance.xlsx")
write.xlsx(list_method[["nnet"]][["result_SA"]], file = "SA.xlsx")
write.xlsx(list_method[["nnet"]][["eff_vector"]], file = "eff.xlsx")
write.xlsx(list_method[["nnet"]][["ranking_order"]], file = "rank.xlsx")
write.xlsx(list_method[["nnet"]][["data_scenario_list"]], file = "data_sce.xlsx")
write.xlsx(list_method[["nnet"]][["metrics_list"]], file = "metrics.xlsx")

write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.75"]]), file = "peer75.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.85"]]), file = "peer85.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_list"]][["0.95"]]), file = "peer95.xlsx")

write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.75"]]), file = "peer_w75.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.85"]]), file = "peer_w85.xlsx")
write.xlsx(as.data.frame(list_method[["nnet"]][["peer_weight_list"]][["0.95"]]), file = "peer_w95.xlsx")

#
list_method[["nnet"]][["peer_list"]][["0.75"]] == list_method[["nnet"]][["peer_weight_list"]][["0.75"]]
list_method[["nnet"]][["peer_list"]][["0.85"]] == list_method[["nnet"]][["peer_weight_list"]][["0.85"]]
list_method[["nnet"]][["peer_list"]][["0.95"]] == list_method[["nnet"]][["peer_weight_list"]][["0.95"]]

model <- list_method[["nnet"]][["final_model"]]
data_train <- list_method[["nnet"]][["final_model"]][["trainingData"]][,-1]

eff_vector <- apply(data_train, 1, function(row) {

  row_df <- as.data.frame(t(row))
  colnames(row_df) <- names(data_train)

  pred <- unlist(predict(model, row_df, type = "prob")[1])

  return(pred)
})

eff_vector <- as.data.frame(eff_vector)

id <- as.data.frame(c(1:nrow(data_train)))
names(id) <- "id"
eff_vector <- cbind(id, eff_vector)

eff_vector$unit <- "real"
eff_vector$unit[98:233] <- "synthetic"
eff_vector$unit <- as.factor(eff_vector$unit)
eff_vector$histogram <- cut(
  eff_vector$eff_vector,
  breaks = seq(0, 1, by = 0.1),  # Intervalos de 0.1
  include.lowest = TRUE          # Incluir el límite inferior
)

library(ggplot2)
ggplot(data = eff_vector, aes(x = histogram, fill = unit)) +
  geom_bar(color = "black", alpha = 0.8, position = "stack") + # Cambiar a posición "stack"
  scale_fill_manual(
    values = c("real" = "orange", "synthetic" = "darkgreen")
  ) +
  labs(
    title = "Train Data efficiencies: Real (97) VS Synthetic (136)",
    x = "x",
    y = "Frecuency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas para mejor legibilidad
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centrar título
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggplot(data = eff_vector[eff_vector$unit == "synthetic",], aes(x = eff_vector, fill = unit)) +
  geom_density(alpha = 0.8, color = "black") + # Cambiar a densidad
  scale_fill_manual(
    values = c("real" = "orange", "synthetic" = "darkgreen")
  ) +
  labs(
    title = "Train Data Efficiencies: Real (97) VS Synthetic (136)",
    x = "x",
    y = "Density",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas para mejor legibilidad
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Centrar título
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

length(which(eff_vector$eff_vector[eff_vector$unit == "real"] < 0.25))



# ## train NN KERAS
# # load keras library and others
# library(keras)
# library(tidyverse)
#
# k_data <- list_method[["svmPoly"]][["finalModel"]][["trainingData"]]
# nrow(k_data)
#
# names(k_data)[1] <- "ClassEfficiency"
#
# k_data <- k_data[,c(2:length(k_data), 1)]
#
# k_data$ClassEfficiency <- as.numeric(k_data$ClassEfficiency)
# # 1 efficient; 2 #ineficient
#
# k_data$ClassEfficiency <- k_data$ClassEfficiency - 1
#
# k_folds <- createFolds(k_data$ClassEfficiency, k = trControl$number)
#
# k_x <- 1:length(x)
# k_y <- (length(x) + 1):(length(x) + length(y))
#
#
#
# fold <- 1
# for (fold in 1:length(k_folds)) {
#
#   # dataset of CV
#   index_fold <- k_folds[[fold]]
#
#   # # separating into train and test
#   # index <- sample(2, nrow(k_data_cv), replace = TRUE, prob = c(train_threshold, 1 - train_threshold))
#
#   x_train <- as.matrix(k_data[-index_fold, c(k_x, k_y)])
#   y_train <- k_data[-index_fold, max(k_y) + 1]
#
#   x_test <- as.matrix(k_data[index_fold, c(k_x, k_y)])
#   y_test <- k_data[index_fold, max(k_y) + 1]
#
#   # save predictions to create confusion matrix
#   y_test01 <- y_test
#
#   y_train <- to_categorical(y_train, 2) #4 categorias
#   y_test <- to_categorical(y_test, 2)
#
#
# }







# # ====== #
# # server #
# # ====== #
#
# file <- paste(DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(simulaciones, file = file)
#
# file_information <- paste("information_", DGP, "_", scenario_char, "_", N_char, "_", noise_char, ".RData", sep = "")
# save(list_information, file = file_information)
