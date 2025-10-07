########################################
# Generate results for DEA evaluations #
########################################

# ------------------------------------------------------------------------------
# Load libraries and package ---------------------------------------------------
# ------------------------------------------------------------------------------
# Package PEAXAI
devtools::load_all()

# Visualize data
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# Load datasets ----------------------------------------------------------------
# ------------------------------------------------------------------------------
load("articulo 1 XAI/code paper/Comparation diferent models/save_datasets_comparation.Rdata")

# ------------------------------------------------------------------------------
# Logistic Regression  ---------------------------------------------------------
# ------------------------------------------------------------------------------
# Get the labeled dataset
data_original <- save_data[[2]]

# changing dataset by imbalance rate
data <- save_data[[2]]
data <- save_data[[3]][["0.2"]]
# data <- save_data[[3]][["0.5"]]

# change ordel levels
data$class_efficiency <- factor(
  data$class_efficiency,
  levels = c("not_efficient", "efficient"))

# determine inputs/outputs
x <- 1
y <- 2
yD <- 3

# determine direction
direction <- c(0,1)

# Get TRUE dataset for predictions
data_yD <- save_data[[1]][,c(x,yD)]
names(data_yD) <- names(data)[c(x,y)]

# fit model
model <- glm(class_efficiency~., 
             data = data, 
             family = binomial)

summary(model)

model <- step(model, direction = "both", trace = TRUE)
summary(model)

# prediction
pred <- predict(
  model,
  newdata = data[, c(x,y)],
  type = "response")

# ------------------------------------------------------------------------------
# Draw model's probabilities ---------------------------------------------------
# ------------------------------------------------------------------------------
xr <- range(data[, x])
yr <- range(data[, y])
length_n <- 250
gx <- seq(0, 10, length.out = length_n)
gy <- seq(0, 10, length.out = length_n)

grid_xy <- expand.grid(
  x1 = gx,
  y  = gy
)
pred <- predict(
  model,
  newdata = grid_xy[, c(x,y)],
  type = "response")

grid_xy$pred <- as.matrix(pred)

# --- 1) Asegura que 'pred' es la columna correcta (prob. de la clase positiva) ---
vars <- names(data)[c(x, y)]

pred_grid <- predict(model, newdata = grid_xy[, vars, drop = FALSE], type = "response")
pos_class <- "efficient"
grid_xy$pred <- as.numeric(pred_grid)

# --- 2) Define los niveles (0.1, 0.2, ..., 0.9) ---
brks <- seq(0.75, 0.95, by = 0.1)
# brks <- c(0.25, 0.5, 0.75, 0.90, 0.95)
ggplot() +
  # Fondo de probabilidades
  geom_raster(
    data = grid_xy,
    aes(x = x1, y = y, fill = pred),
    interpolate = FALSE,
    alpha = 0.6
  ) +
  scale_fill_gradient2(
    low = "#F59AA3", mid = "white", high = "#8FD7A5",
    midpoint = 0.5, limits = c(0, 1), oob = squish,
    name = "P(y=1)"
  ) +
  
  # Curvas de nivel 0.1, 0.2, ..., 0.9
  geom_contour(
    data = grid_xy,
    aes(x = x1, y = y, z = pred),
    breaks = brks,
    colour = "grey30",
    linewidth = 0.4
  ) +
  
  # # Resalta la isoprobabilidad 0.5 (frontera de decisión)
  # geom_contour(
  #   data = grid_xy,
  #   aes(x = x1, y = y, z = pred),
  #   breaks = 0.5,
  #   colour = "black",
  #   linewidth = 0.8
  # ) +
  
  # (opcional) puntos y línea de referencia
  geom_point(
    data = data_original, aes(x = x1, y = y),
    color = "black", size = 1, inherit.aes = FALSE
  ) +
  geom_line(
    data = save_data[[1]], aes(x = x1, y = yD),
    color = "black", alpha = 0.5, linewidth = 1, linetype = "dashed",
    inherit.aes = FALSE
  ) +
  
  coord_cartesian(xlim = range(grid_xy$x1), ylim = range(grid_xy$y), expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# Calculate betas values -------------------------------------------------------
# ------------------------------------------------------------------------------
betas_Logit <- compute_target(
  data = data_original[,c(x,y)],
  x = x,
  y = y,
  final_model = model,
  cut_off = 0.999,
  imp_vector = direction
)[["betas"]][,1]


