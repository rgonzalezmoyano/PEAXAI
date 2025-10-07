### Generate an image of the SVM hyperplane.
library(caret)
set.seed(6)

data <- matrix(
  data = NA, ncol = 4, nrow = 20)

data <- as.data.frame(data)

names(data) <- c("DMU", "x", "y", "class")

# complete de data
# DMU
data$DMU <- 1:nrow(data)

# class
data$class[1:(nrow(data)/2)] <- "red" 
data$class[((nrow(data)/2)+ 1):nrow(data)] <- "blue"

data$class <- as.factor(data$class)

# x and y
# Generar datos para clasificación en dos clases


# Número total de observaciones
n <- length(data$DMU)

# Generar datos para clase 1
x1 <- rnorm(n/2, mean = 2, sd = 1)
y1 <- rnorm(n/2, mean = 2, sd = 1)

# Generar datos para clase 2
x2 <- rnorm(n/2, mean = 6, sd = 1)
y2 <- rnorm(n/2, mean = 6, sd = 1)

# Combinar los datos
x <- c(x1, x2)
y <- c(y1, y2)
data$x <- x
data$y <- y

# Graficar los datos
plot(data$x, data$y, col = data$class, pch = 19, main = "Datos de Clasificación")
legend("topright", legend = c("Clase 1", "Clase 2"), col = c(1, 2), pch = 19)


# change to better plot
original_point <- data[1,]

data[1, 3] <- 3.90

original_point <- data[4,]

data[4, 3] <- 2.01

original_point <- data[4,]

data[14, 3] <- 5.2


# train
trControl <- trainControl (
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "all"
)

modelo <- train (
  form = class ~ .,
  data = data[-1],
  method = "nnet",
  trControl = trControl,
  tuneGrid = expand.grid(size = c(3:5), decay = c(0, 0.1, 0.01, 0.001)),
  maxit = 1000
) 

library(NeuralNetTools)
old.par <- par(mar = c(bottom = 0, left = 2, top = 2, right = 3), xpd = NA)
plot <- plotnet(modelo,
        bias = F,                 # No mostrar bias
        pos_col = "black",             # Color de pesos positivos
        neg_col = "black",             # Color de pesos negativos
        var_labs = FALSE,              # No mostrar etiquetas de las variables
        nid = F)                   # No mostrar grosor de las conexiones
# old.par <- par(mar = c(bottom = 0, left = 2, top = 2, right = 3), xpd = NA)

plot
a <- plot(modelo)
a$formula

ggsave(plot = plot, dpi = 600, filename = "nnet  graff 2 3 1.png")
summary(modelo)
gaaspect.fillgarson(modelo)


### grid


grid <- expand.grid (
  x = seq(0, 9, length = 300),
  y = seq(0, 9, length = 300)
)

grid$decision <- predict(modelo, grid, type = "raw")

plot <- ggplot() +
  
  # # predictions
  geom_point(data = grid, aes(x = x, y = y, colour = decision), size = 0.6, alpha = 0.1) +
  
  # original points
  geom_point(data = data, aes(x = x, y = y, color = class), size = 2.5) +
  
  # # support vectors
  # geom_point(data = data[sv, ], aes(x = x, y = y, color = class), size = 5) +
  
  # # hyperplane
  # geom_segment(data = svm[1,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1) +
  # 
  # # margin top
  # geom_segment(data = svm[2,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, linetype  = "dashed") +
  # 
  # # margin bottom
  # geom_segment(data = svm[3,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, linetype  = "dashed") +
  
  # color class
  scale_color_manual(values = c("deepskyblue1", "red"), name = "Class") +
  
  # names exes
  xlab("Feature 1") +
  ylab("Feature 2") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

ggsave(plot = plot, dpi = 600, filename = "nnet clasification example.png")  
