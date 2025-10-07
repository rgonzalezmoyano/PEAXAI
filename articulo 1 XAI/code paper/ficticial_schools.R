# necesary to have the data

# new 3 schools

# names cols
names_data <- names(data)

# data structure
new_data <- as.data.frame (
  matrix(
    data = NA,
    ncol = ncol(data),
    nrow = 3
  )
)

names(new_data) <- names(data)

# save netrics to use
# mean
mean_data <- colMeans(data[,c(x,y)])

#ESCS in 25 percentil
ESCS_percentil <- quantile(data$ESCS_m2, probs = seq(0, 1, by = 0.05))

# PVMATH percentil
PVMATH_percentil <- quantile(data$PVMATH_m, probs = seq(0, 1, by = 0.05))

# mean by default
new_data$PVSCIE_m <- mean_data[4]
new_data$PVMATH_m <- mean_data[5]
new_data$PVREAD_m <- mean_data[6]

new_data$EDUSHORT2 <- mean_data[3]
new_data$TSRATIO <- mean_data[2]
new_data$ESCS_m2 <- mean_data[1]

# to matrix
new_data <- as.matrix(new_data)

# 1st school
n <- 1
new_data[n, 10] <- ESCS_percentil[6]

# 2nd school
n <- 2
new_data[n, 10] <- ESCS_percentil[19]
new_data[n, 4] <- PVMATH_percentil[6]

# 3rd school
n <- 3
new_data[n, 10] <- ESCS_percentil[16]

# to data.frame
new_data <- as.data.frame(new_data)

# SCHLTYPE is 3
new_data$SCHLTYPE <- 3

# Region is madrid
new_data$Region <- 17

new_data$Region <- as.factor(new_data$Region)
new_data$SCHLTYPE <- as.factor(new_data$SCHLTYPE)

# Classify new_data
model_SVM <- information_region[[2]][[1]][[1]]
model_NN <- information_region[[2]][[2]][[1]]


a <- predict(model_SVM, new_data, type = "prob")
a

b <- predict(model_NN, new_data, type = "prob")
b

scores_SVM <- compute_scores (
  data = new_data,
  x = x,
  y = y,
  z = z,
  final_model = model_SVM,
  orientation = orientation,
  cut_off = model_SVM$cut_off
)  

scores_NN <- compute_scores (
  data = new_data,
  x = x,
  y = y,
  z = z,
  final_model = model_NN,
  orientation = orientation,
  cut_off = model_NN$cut_off
) 

