
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
