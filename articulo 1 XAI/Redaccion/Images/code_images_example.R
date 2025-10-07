# Toy example

# libraries

devtools::load_all()
library(caret)
library(Benchmarking)
library(magrittr)
library(dplyr)
library(deaR)
library(haven)
library(e1071)
library(rminer)

library(ggplot2)
library(rminer)


# Configuración
seed <- 0
seed <- 2
set.seed(seed)
n <- 30
n<-40
# Generación de inputs x1 e x2
x1 <- runif(n, 1, 10)
x2 <- runif(n, 1, 10)

# Generar una función de producción con curvatura
A <- 1.5
Q_star <- A * (x1^0.5 + x2^0.5)  # Función de frontera más curvada (convexa)

# Introducir ineficiencia
inefficiency <- runif(n, 0.7, 1)  # Factor de ineficiencia
Q <- Q_star * inefficiency        # Q observada

# Crear un DataFrame
data <- data.frame(x = x1, x2 = x2, y = Q, inefficiency = inefficiency)
data_gra <- data

# Gráfico de dispersión de x1 y Q (proyección en 2D)
ggplot(data) +
  geom_point(aes(x = x1, y = Q), color = "blue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, max(Q_star) * 1.1)) +
  theme_bw() +
  labs(title = "Dataset toy-example 30 DMUs",
       x = "y",
       y = "y")

data <- data[, c(1,3)]

# x and y indexes
x <- 1
y <- 2
#z <- c(2, 8) # environment variables

# different types to label
target_method <- "additive"

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

# SMOTE proportions
balance_data <- c(seq(0.20, 0.4, 0.05))

# ML metric
metric = "F"

# scenarios to peer
scenarios <- c(seq(0.75, 0.95, 0.1)) # seq(0.75, 0.95, 0.1)

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

hold_out <- 0.00 # https://topepo.github.io/caret/train-models-by-tag.html

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

### graph
### determinate efficient class BALANCED INPUT

data <- list_method[["nnet"]][["final_model"]][["trainingData"]]
data <- cbind(data[-1], data[1])
names(data) <- c("x", "y", "class_efficiency")

eval_data <- data[1:n,]
efficient_data <- data[data$class_efficiency == "efficient", ]

plot1 <- ggplot() +
  
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  
  # name DMUs
  geom_text(data = eval_data[eval_data$class_efficiency == "efficient", ],
            aes(x = x, y = y, label = row.names(eval_data[eval_data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = eval_data, aes(x = x, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14),   # Tamaño de los números en los ejes
    plot.title = element_text(size = 18),  # Por si añades un título general
    legend.text = element_text(size = 14), # Texto de la leyenda
    legend.title = element_text(size = 15) # Título de la leyenda
  )

plot1

#ggsave(plot = plot1, dpi = 600, filename = "DEA_label_efficient.png")
ggsave(file = "DEA_label_efficient.png", plot = plot1, dpi = 600, width = 10, heigh = 6)

### plot 2 training dataset

new_dmu <- data[41:50,]
rownames(new_dmu) <- c(41:50)

plot2 <- ggplot() +
  
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  
  # name DMUs
  geom_text(data = eval_data[eval_data$class_efficiency == "efficient", ],
            aes(x = x, y = y, label = row.names(eval_data[eval_data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +

  # # name DMUs
  # geom_text(data = new_dmu[new_dmu$class_efficiency == "efficient", ],
  #           aes(x = x, y = y, label = row.names(new_dmu[new_dmu$class_efficiency == "efficient", ])),
  #           vjust = -1, hjust = 1) +
  
  geom_point(data = data, aes(x = x, y = y, color = class_efficiency)) +
  
  #geom_point(data = eval_data, aes(x = x, y = y)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14),   # Tamaño de los números en los ejes
    plot.title = element_text(size = 18),  # Por si añades un título general
    legend.text = element_text(size = 14), # Texto de la leyenda
    legend.title = element_text(size = 15) # Título de la leyenda
  )

plot2

#ggsave(plot = plot2, dpi = 600, filename = "DEA_new_efficient.png")
ggsave(file = "DEA_new_efficient2.png", plot = plot2, dpi = 600, width = 10, heigh = 6)


# ============= #
# Generate plot #
# ============= #

# make a grid of the predictors
grid <- expand.grid (
  x = seq(0, 10, length = 300),
  y = seq(0, 10, length = 300)
)

model <- list_method[["nnet"]][["final_model"]][["finalModel"]]
grid$label <- predict(model, grid, type = "class") #"raw"

grid$label <- factor(grid$label)


plot5 <- ggplot(data = eval_data) +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  
  geom_point(data = grid, aes(x = x, y = y, color = label), size = 0.6, alpha = 0.5) + #  size = 0.6, alpha = 0.3
  geom_point(aes(x = x, y = y)) +
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +

  # name DMUs
  geom_text(data = eval_data[eval_data$class_efficiency == "efficient", ],
            aes(x = x, y = y, label = row.names(eval_data[eval_data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  labs(x = "input", y = "output") +

  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14),   # Tamaño de los números en los ejes
    plot.title = element_text(size = 18),  # Por si añades un título general
    legend.text = element_text(size = 14), # Texto de la leyenda
    legend.title = element_text(size = 15) # Título de la leyenda
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)))

plot5


ggsave(plot = plot5, dpi = 600, width = 10, heigh = 6, filename = "balance_class.png")

# draw a directional vector
# Coordenadas de los puntos
DMU <- 22
point <- data[DMU,1:2]
projection <- list_method[["nnet"]][["data_scenario_list"]][[1]][["data_scenario"]][DMU,]

names(projection) <- names(point)

# Crear un data frame para el vector
vector_data <- data.frame(
  x = point[,1],         # Coordenada x inicial
  y = point[,2],         # Coordenada y inicial
  xend = projection[,1],      # Coordenada x final
  yend = projection[,2]       # Coordenada y final
)

plot7 <- ggplot(data = eval_data) +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  
  geom_point(data = grid, aes(x = x, y = y, color = label), size = 0.6, alpha = 0.2) + 
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +
  
  geom_point(aes(x = x, y = y), alpha = 0.4) +
  geom_point(data = projection, aes(x = x, y = y)) +
  geom_point(data = point, aes(x = x, y = y)) +
  
  geom_segment(
    data = vector_data, aes(x = x, y = y, xend = xend + 0.03, yend = yend - 0.05),
    arrow = arrow(length = unit(0.2, "cm")), # Flecha al final del segmento
    size = 0.7,
    color = "blue") +
  
  # name DMUs
  geom_text(data = eval_data[DMU, ],
            aes(x = x, y = y, label = DMU),
            vjust = +1, hjust = 1.2) +
  
  # name DMUs
  geom_text(data = projection,
            aes(x = x, y = y, label = "22'"),
            vjust = -0.6, hjust = 1.2) +
  
  # name DMUs
  geom_text(data = eval_data[eval_data$class_efficiency == "efficient", ],
            aes(x = x, y = y, label = row.names(eval_data[eval_data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +

  labs(x = "input", y = "output") +
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14),   # Tamaño de los números en los ejes
    plot.title = element_text(size = 18),  # Por si añades un título general
    legend.text = element_text(size = 14), # Texto de la leyenda
    legend.title = element_text(size = 15) # Título de la leyenda
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)))

plot7

ggsave(plot = plot7, dpi = 600, width = 10, heigh = 6, filename = "projection.png")

# degradado
# make a grid of the predictors
grid <- expand.grid (
  x = seq(0, 10, length = 700),
  y = seq(0, 10, length = 700)
)

model <- list_method[["nnet"]][["final_model"]]
grid$label <- unlist(predict(model, grid, type = "prob")[1]) #"raw"

#grid$label <- factor(grid$label)

plot6 <- ggplot() +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  
  geom_point(data = grid, aes(x = x, y = y, color = label), size = 1, alpha = 1) +

  labs(x = "input", y = "output", color = "Prob.") +
  
  scale_color_gradientn(
    colors = c("white", "black", "black", "white"), # Azul para 0, blanco para 0.5, rojo para 1 olivedrab2 pink
    values = c(0, 0.25,0.75, 1), # Especifica las posiciones relativas de los colores
    limits = c(0, 1)) + # Límites de la variable
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Tamaño de los títulos de los ejes
    axis.text = element_text(size = 14),   # Tamaño de los números en los ejes
    plot.title = element_text(size = 18),  # Por si añades un título general
    legend.text = element_text(size = 9), # Texto de la leyenda
    legend.title = element_text(size = 12) # Título de la leyenda
  )

plot6

ggsave(plot = plot6, dpi = 600, width = 10, heigh = 6, filename = "NN_uncertenty.png")
library(openxlsx)
# write.xlsx(list_method[["nnet"]][["real_decision_balance"]], file = "real_decision_balance_example.xlsx")
# write.xlsx(list_method[["nnet"]][["train_decision_balance"]], file = "train_decision_balance_example.xlsx")  
#write.xlsx(list_method[["nnet"]][["ranking_order"]], file = "ranking_example.xlsx")






