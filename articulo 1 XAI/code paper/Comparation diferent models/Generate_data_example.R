#New code paper1 valencian example
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
library(fastDummies)
library(keras)
#library(MLmetrics)

# ===
# Generate example data
# ===
# generate data
set.seed(1997)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 50,
    nX = 1
  )
)

# plot
ggplot() +
  geom_point(data = data, aes(x = x1, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(0, 10)) +  # Límites para x
  scale_y_continuous(limits = c(0, 10)) +  # Límites para y
  theme_bw() +
  theme(legend.position = "bottom")

original_data <- data

# ===
# Information to cafee
# ===

# x and y indexes
x <- 1
y <- 2
z <- NULL

# different types to label
# target_method <- "additive"
# convexity = TRUE
# returns = "variable"

# Step 1: label data
label_efficiency <- label_efficiency(
  data = data,
  x = x,
  y = y
)

sum(label_efficiency$data_proportions$n_efficient)

# training phase
# create a validation dataset
# Create train and validation data
hold_out <- NULL # https://topepo.github.io/caret/train-models-by-tag.html

# set seed 
seed <- 0

# # Crear índice de validación
# valid_index <- createDataPartition(
#   label_efficiency[["data_labeled"]]$class_efficiency,
#   p = hold_out,
#   list = FALSE
# )
valid_index <- NULL


set.seed(0)

# Dividir dataset en entrenamiento y validación
valid_data <- label_efficiency[["data_labeled"]]
train_data <- label_efficiency[["data_labeled"]]
new_x <- label_efficiency[["index"]][["x"]]
new_y <- label_efficiency[["index"]][["y"]]
new_z <- label_efficiency[["index"]][["z"]]


prop.table(table(train_data$class_efficiency))

# addresing imbalance
balance <- c(seq(0.20, 0.5, 0.05 )) # c(NA, 0.2, 0.3, 0.4, 0.5)

train_data_SMOTE <- SMOTE_data(
  data = train_data,
  x = new_x,
  y = new_y,
  z = new_z,
  balance_data = balance
)


copy_train_data <- train_data
copy_train_data_SMOTE <- train_data_SMOTE
copy_valid_data <- valid_data
save_data <- list(original_data, copy_train_data, copy_train_data_SMOTE, copy_valid_data, label_efficiency, valid_index)
# save(save_data, file = "save_datasets_comparation.Rdata")

load("articulo 1 XAI/code paper/Comparation diferent models/save_datasets_comparation.Rdata")