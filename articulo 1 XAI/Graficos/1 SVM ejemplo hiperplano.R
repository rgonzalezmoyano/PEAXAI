### Generate an image of the SVM hyperplane.

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

# train model

trControl <- trainControl (
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "all"
)

model <- train(
  data = data,
  class ~ x + y,
  method = "svmLinear",
  tuneGrid  = expand.grid(C = c(1)),
  trControl = trControl)

sv <- model[["finalModel"]]@SVindex

# change to better plot
original_point <- data[1,]

data[1, 3] <- 3.90

original_point <- data[4,]

data[4, 3] <- 2.01

original_point <- data[4,]

data[14, 3] <- 5.2

# draw
rng.x <- range(data$x)

rng.y <- range(data$y)

grid <- expand.grid (
  x1 = seq(0, 9, length = 300),
  y = seq(0, 9, length = 300)
)

names(grid) <- c("x", "y")

grid$decision <- predict(model, grid, type = "raw")

svm <- as.data.frame(
  matrix(
    data = NA, ncol = 4, nrow = 3)
)

rownames(svm) <- c("hyplane", "margin top", "margin bottom")
names(svm) <- c("x", "y", "xend", "yend")

inc <- 1

svm$x <- c(7.068942, 7.068942 + inc, 7.068942 - inc)
svm$y <- c(0, 0 + inc, 0 - inc)
svm$xend <- c(1.173500, 1.173500 + inc, 1.173500 - inc)
svm$yend <- c(7.61695970, 7.61695970 + inc, 7.61695970 - inc)

plot <- ggplot() +
  
  # # predictions
  # geom_point(data = grid, aes(x = x, y = y, colour = decision), size = 0.5, alpha = 0.5) +

  # original points
  geom_point(data = data, aes(x = x, y = y, color = class), size = 2) +

  # support vectors
  geom_point(data = data[sv, ], aes(x = x, y = y, color = class), size = 5) +
  
  # hyperplane
  geom_segment(data = svm[1,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1) +
  
  # margin top
  geom_segment(data = svm[2,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, linetype  = "dashed") +
  
  # margin bottom
  geom_segment(data = svm[3,], aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 1, linetype  = "dashed") +
 
  # color class
  scale_color_manual(values = c("deepskyblue1", "red")) +
  
  # names exes
  xlab("Feature 1") +
  ylab("Feature 2") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "svm clasification example.png")
