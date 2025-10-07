#New code paper1 valencian example
# ------------------------------------------------------------------------------
# Load libraries ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# devtools::load_all()
library(Benchmarking)
library(writexl)

# ------------------------------------------------------------------------------
# Load data --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ======================== #
# Valencian Community 2018 #
# ======================== #
load("articulo 1 XAI/data_valencia_comunity/firms.RData")
data <- firms

# save a copy
data_original <- data

# # make changes realted to class
# data <- change_class(data = data, to_factor = c(5,6))

# filter to valencian comunity
data <- data[data$autonomous_community == "Comunidad Valenciana",]

# ------------------------------------------------------------------------------
# DEA parameters ---------------------------------------------------------------
# ------------------------------------------------------------------------------
# x, y, z
x <- c(9:12)
y <- c(8)
z <- NULL

# ------------------------------------------------------------------------------
# DEA-DDF ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# directional vectors
g_1 <- c(1, 1, 1, 1, 1)
# g_mean <- colMeans(data[,c(x,y)])

# result g_1
model_dea <- dea.direct(
  X = data[, x],
  Y = data[, y],
  XREF = data[, x],
  YREF = data[, y],
  DIRECT = g_1,
  RTS = "vrs",
  ORIENTATION = "in-out",
  SLACK = FALSE)

result_g_1 <- model_dea[["objval"]]
mean(result_g_1)
projections_x <- data[,c(x)] - t((g_1[1:4] %o% model_dea[["objval"]]))
projections_y <- data[,c(y)] + t((g_1[5] %o% model_dea[["objval"]]))

colMeans(projections_x)
colMeans(projections_y)

sd(projections_x$total_assets)
sd(projections_x$employees)
sd(projections_x$fixed_assets)
sd(projections_x$personal_expenses)
sd(projections_y)

sd(model_dea[["objval"]])



# result g_mean
model_dea <- dea.direct(
  X = data[, x],
  Y = data[, y],
  XREF = data[, x],
  YREF = data[, y],
  DIRECT = g_mean,
  RTS = "vrs",
  ORIENTATION = "in-out",
  SLACK = FALSE)

result_g_mean <- model_dea[["objval"]]

mean(result_g_mean)
projections_x <- data[,c(x)] - t((g_mean[1:4] %o% model_dea[["objval"]]))
projections_y <- data[,c(y)] + t((g_mean[5] %o% model_dea[["objval"]]))

colMeans(projections_x)
colMeans(projections_y)

sd(projections_x$total_assets)
sd(projections_x$employees)
sd(projections_x$fixed_assets)
sd(projections_x$personal_expenses)
sd(projections_y)

sd(model_dea[["objval"]])


# special case
DMUs <- 1:nrow(data)

result_g_X0_Y0 <- c()
project_result <- as.data.frame(matrix(
  data = NA,
  ncol = length(x) + length(y),
  nrow = nrow(data)
  ))
names(project_result) <- names(data[,c(x,y)])


for (unit in DMUs) {
  
  g_X0_Y0 <- as.vector(as.matrix(data[unit, c(x,y)]))
  
  # result g_X0_Y0 
  model_dea <- dea.direct(
    X = data[unit, x],
    Y = data[unit, y],
    XREF = data[, x],
    YREF = data[, y],
    DIRECT = g_X0_Y0,
    RTS = "vrs",
    ORIENTATION = "in-out",
    SLACK = FALSE)
  
  result_g_X0_Y0[unit] <- model_dea[["objval"]]
  
  projections_x <- data[unit,c(x)] - t((g_X0_Y0[1:4] %o% model_dea[["objval"]]))
  projections_y <- data[unit,c(y)] + t((g_X0_Y0[5] %o% model_dea[["objval"]]))
  
  project_result[unit,c(1:4)] <- projections_x
  project_result[unit,5] <- projections_y
}
sd(result_g_X0_Y0)
median(project_result$total_assets)
mean(result_g_X0_Y0)

colMeans(project_result)

export <- list(as.data.frame(result_g_1), as.data.frame(result_g_mean), as.data.frame(result_g_X0_Y0))
names(export) <- c("g_1", "g_mean", "g_g_X0_Y0")

# ------------------------------------------------------------------------------
# Export to Excel --------------------------------------------------------------
# ------------------------------------------------------------------------------
# write_xlsx(export, path = "betas_DEA.xlsx")








