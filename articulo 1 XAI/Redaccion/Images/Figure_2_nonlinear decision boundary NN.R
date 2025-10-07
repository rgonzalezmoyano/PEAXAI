# Cargar librerías necesarias
library(ggplot2)
library(nnet)  # Para la red neuronal
library(dplyr)

# Definir rango de valores para asegurar un gráfico cuadrado
set.seed(123)
n <- 20  # Número de puntos por clase

# Clase 0 (azul) en la parte superior izquierda de una frontera curva
x1_class0 <- runif(n, -3, 3)
x2_class0 <- runif(n, x1_class0^2 - 2, 3)  # Frontera cuadrática más suave
y_class0 <- rep(0, n)

# Clase 1 (rojo) en la parte inferior derecha
x1_class1 <- runif(n, -3, 3)
x2_class1 <- runif(n, -3, x1_class1^2 - 2)  # Frontera cuadrática más suave
y_class1 <- rep(1, n)

# Combinar los datos en un data frame
data <- data.frame(
  x1 = c(x1_class0, x1_class1),
  x2 = c(x2_class0, x2_class1),
  y = as.factor(c(y_class0, y_class1))
)

# Filtrar los datos para mantener dentro de los límites [-3, 3]
data <- data %>% filter(x1 >= -3 & x1 <= 3 & x2 >= -3 & x2 <= 3)

# Entrenar la red neuronal con nnet
nn_model <- nnet(y ~ x1 + x2, data = data, size = 5, maxit = 500, decay = 0.1)

# Crear una malla de puntos para visualizar la frontera de decisión
grid_size <- 300
x1_seq <- seq(-3, 3, length.out = grid_size)
x2_seq <- seq(-3, 3, length.out = grid_size)
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

# Predecir las clases en la malla y convertirlas a -1 y 1
grid$y_pred <- as.numeric(predict(nn_model, grid, type = "class"))  # Convertir factor a numérico
grid$y_pred <- ifelse(grid$y_pred == 1, 1, -1)  # Reasignar valores 1 -> 1 y 0 -> -1

# Graficar la frontera de decisión con ggplot2
plot <- ggplot(data, aes(x = x1, y = x2)) +
  geom_tile(data = grid, aes(fill = as.factor(y_pred)), alpha = 0.3) +  # Superficie de decisión
  geom_point(aes(color = y), size = 2) +  # Puntos de datos originales
  scale_fill_manual(values = c("blue", "red"), name = "Predicted Class", labels = c("-1", "1")) +
  scale_color_manual(values = c("blue", "red"), name = "Actual Class",  labels = c("-1", "1")) +
  #coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +  # Limitar el gráfico a [-3, 3]
  theme_minimal() +
  # Dibujar un rectángulo en los límites del gráfico
  geom_rect(aes(xmin = min(grid$x1), xmax = max(grid$x1), ymin = min(grid$x2), ymax = max(grid$x2)), 
            fill = NA, color = "black", size = 0.5) +
  labs(x = "Feature 1",
       y = "Feature 2") +
  theme(legend.position = "bottom",
        axis.title = element_text(color = "black", size = 14),  # Color negro para los nombres de los ejes
        axis.text = element_text(color = "black", size = 12)) +   # Asegurar que los números también sean negros) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) 
  
plot <- ggplot(data, aes(x = x1, y = x2)) +
  geom_tile(data = grid, aes(fill = as.factor(y_pred)), alpha = 0.3) +  # Superficie de decisión
  geom_point(aes(color = y), size = 2) +  # Puntos de datos originales
  scale_fill_manual(values = c("blue", "red"), name = "Predicted Class", labels = c("-1", "1")) +
  scale_color_manual(values = c("blue", "red"), name = "Actual Class", labels = c("-1", "1")) +
  # coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +  # Limitar el gráfico a [-3, 3]
  theme_minimal() +
  geom_rect(aes(xmin = min(grid$x1), xmax = max(grid$x1), ymin = min(grid$x2), ymax = max(grid$x2)), 
            fill = NA, color = "black", size = 0.5) +
  labs(x = "Feature 1",
       y = "Feature 2") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(color = "black", size = 14),   # Tamaño de títulos de los ejes
    axis.text = element_text(color = "black", size = 12),    # Tamaño de los números de los ejes
    legend.title = element_text(size = 13),                  # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),                   # Tamaño del texto de la leyenda
    plot.title = element_text(size = 16, face = "bold")      # Tamaño del título del gráfico (si lo usas)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))  # Leyenda horizontal

plot

ggsave(file = "nonlinear decision boundary NN.png", plot = plot, dpi = 600, width = 10, heigh = 6, bg = "white")





ggsave(file = "img1.png", plot = img1, dpi = 600, width = 10, heigh = 6)
