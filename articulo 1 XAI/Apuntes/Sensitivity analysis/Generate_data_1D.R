### Example simulation
devtools::load_all()

# libraries
library("ggplot2")
library("rminer")

# generate data
set.seed(1997)

# Simulated data
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 1
  )
)

# plot
ggplot() +
  geom_point(data = data, aes(x = x1, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

# 
# library(openxlsx)
# write.xlsx(data, "data_example.xlsx")

# x and y indexes
x <- 1
y <- 2
z <- NULL

# import


# different types to label
target_method <- "BCC"

set.seed(314)
methods <- list (
  # "svmPoly" = list(
  #   hyparams = list(
  #     "degree" = c( 2),
  #     "scale" = c(1),
  #     "C" = c(1, 10)
  #   )
  # )
  
  # svm
  "svmPoly" = list(
    hyparams = list(
      "degree" = c(3),
      "scale" = c(0.1),
      "C" = c(1)
    )
  )
  # "svmRadial" = list(
  #   hyparams = list(
  #     "sigma" = c(0.01, 0.1, 1, 10, 100),
  #     "C" = c(0.001, 0.1, 1, 10, 100)
  #   )
  # ),
  
  # # random forest
  # "rf" = list (
  #   options = list (
  #     ntree = c(500) # c(100, 500, 1000)
  #   ),
  #   hyparams = list(
  #     mtry = c(4)
  #   )
  # ),
  
  # # neuronal network
  # "nnet" = list(
  #   hyparams = list(
  #     "size" = c(1, 5, 10, 20),
  #     "decay" = c(0, 0.1, 0.01, 0.001, 0,0001)
  #   ),
  #   options = list (
  #     maxit = 1000
  #   )
  # )
  
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
orientation <- "output"

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

# Parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.00
# https://topepo.github.io/caret/train-models-by-tag.html

metric = "Accuracy"

convexity <- TRUE
returns <- "variable"

# save scores region
list_region <- list()

# new  dataset of scores result
scores <- matrix (
  ncol = length(methods),
  nrow = nrow(data)
) 

# change to data.frame
scores <- as.data.frame(scores)

# change names
score_names <- names(methods)
names(scores) <- score_names

# save model information
list_method <- list()  

# bucle region
for (i in 1:length(methods)) {
  
  # console information
  print(paste("METODO:", i))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
    orientation = orientation,
    trControl = trControl,
    method = methods[i],
    target_method = target_method,
    metric = metric,
    hold_out = hold_out,
    convexity = convexity,
    returns = returns
  )
  
  # to get probabilities senarios
  data_contr <- as.data.frame(matrix(
    data = NA,
    ncol = ncol(final_model$final_model$trainingData) + length(seq(0.75, 1, 0.05)),
    nrow = nrow(final_model$final_model$trainingData)
  ))
  
  # Copiar las columnas x e y de los datos originales
  data_contr[, c(x, y)] <- as.matrix(final_model$final_model$trainingData[, 1 + c(x, y)])
  
  # Usar apply para hacer las predicciones en cada fila
  data_contr[, y + 1] <- apply(final_model$final_model$trainingData[, 1 + c(x, y)], 1, function(fila) {
    # Convierte la fila en un data frame con nombres de columna apropiados
    nueva_fila <- as.data.frame(t(fila))
    colnames(nueva_fila) <- colnames(final_model$final_model$trainingData)[1 + c(x, y)]
    
    # Predicción con el modelo
    predict(final_model$final_model$finalModel, newdata = nueva_fila)[1]
  })
  
  names(data_contr) <- c(names(data[-length(data)]), "class", c(seq(0.75, 1, 0.05)))
  
  train_data_loop <- final_model$final_model$trainingData[,c(2:length(final_model$final_model$trainingData),1)]
  
  loop <- 1
  for (prob in seq(0.75, 1, 0.05)) {
    print(prob)
    #bset cut off is selected
    scores_cafee <- compute_scores (
      data = train_data_loop[-3],  #data,
      x = x,
      y = y,
      #z = z,
      final_model = final_model$final_model,
      orientation = orientation,
      cut_off = prob #final_model$final_model[["cut_off"]]
    )

    data_contr[, length(final_model$final_model$trainingData) + loop] <- (scores_cafee * min(train_data_loop$y)) 

    loop <- loop + 1
  }

  #scores[i] <- scores_cafee
  
  # # Importance of variables
  # # varImp Caret
  # importance <- varImp(object = final_model$final_model)
  # print(importance)
  # 
  # plot <- plot(importance)
  
  if (names(methods[i]) == "svmPoly") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    train_data <- train_data[,c(2:length(train_data),1)]
    
    # # con rminer pero no escala
    # m <- fit(
    #   ClassEfficiency~.,
    #   data = train_data,
    #   model = "ksvm",
    #   kernel = "polydot",
    #   scale = "none",
    #   kpar = list(
    #     degree = final_model$final_model$bestTune$degree,
    #     scale = final_model$final_model$bestTune$scale
    #   ),
    #   C = final_model$final_model$bestTune$C
    # )
    
    # importance with our model
    mypred <- function(M, data) {
      
      return (predict(M, data[-length(data)], type = "prob"))
      
    }
    
    # Define methods and measures
    methods_SA <- c("1D-SA") # c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD") #  c("AAD", "gradient", "variance", "range")
      
    # Calculate the importance for the current method and measure
    levels <- 5
    importance <- Importance(
      M = final_model$final_model$finalModel,
      RealL = levels, # Levels
      data = train_data, # data
      method = methods_SA,
      measure = measures_SA,
      #sampling = regular,
      baseline = "mean", # mean, median, with the baseline example (should have the same attribute names as data).
      responses = TRUE,
      PRED = mypred,
      outindex = length(train_data)
    )
    
  } else if (names(methods[i]) == "nnet") {
    
    # necesary data to calculate importance
    train_data <- final_model$final_model[["trainingData"]]
    names(train_data)[1] <- "ClassEfficiency"
    
    dataset_dummy <- model.matrix(ClassEfficiency~ . - 1, data = train_data)
    train_data <- cbind(train_data[1], dataset_dummy)
    
    train_data <- train_data[,c(2:length(train_data),1)]
    
    # con rminer
    m <- fit(
      ClassEfficiency ~.,
      data = train_data,
      model = "mlp",
      scale = "none",
      size = final_model$final_model$bestTune$size,
      decay = final_model$final_model$bestTune$decay
    )
    
    # Define methods and measures
    methods_SA <- c("1D-SA", "sens", "DSA", "MSA", "CSA", "GSA")
    measures_SA <- c("AAD", "gradient", "variance", "range")
    
    # make grid SA
    grid_SA <- expand.grid(method = methods_SA, measure = measures_SA)
    
    # save results
    results_SA <- data.frame(method = character(), measure = character())
    
    # Loop through each combination of method and measure
    for (a in 1:nrow(grid_SA)) {
      
      method <- as.character(grid_SA$method[a])
      measure <- as.character(grid_SA$measure[a])
      
      # Calculate the importance for the current method and measure
      importance <- Importance(m, data = train_data, method = method, measure = measure)
      
      # Extract the importance values (assuming 26 values)
      imp_values <- importance$imp
      
      # Create a row with method, measure, and the 26 importance values
      result_row <- data.frame(method = method, measure = measure, t(imp_values))
      
      # Append the row to results_SA
      results_SA <- rbind(results_SA, result_row)
    }
    
    names(results_SA)[3:ncol(results_SA)] <- names(train_data)
    
  }
  
  
  # information model
  list <- list()
  
  list[[1]] <- final_model
  list[[2]] <- importance
  
  names(list) <- c("final_model", "importance")
  
  
  list_method[[i]] <- list
  
} # end bucle for (methods)  

information_region <- list()
information_region[[1]] <- scores
information_region[[2]] <- list_method


names(information_region) <- c("scores","ML_models")

# names final object
names(information_region) <- c("scores","ML_models")

names(information_region[["ML_models"]]) <- names(methods)

# prove
importance_example <- information_region[["ML_models"]][["svmPoly"]][["importance"]]
summary(train_data)


p.x1 <- round(importance_example$sresponses[[2]]$y, 2)[,1] # 2 es el input 1
p.y <- round(importance_example$sresponses[[3]]$y, 2)[,1] # 3 es el output 1

data_prob <- as.data.frame(t(rbind(p.x1,p.y)))
data_prob <- cbind(c(1:5), data_prob)
names(data_prob)[1] <- "level"

data_long <- tidyr::pivot_longer(data_prob, cols = c(p.x1, p.y), names_to = "variable", values_to = "value")


plot_1D <- ggplot(data_long, aes(x = level, y = value, color = variable)) +
  geom_line(size = 1.2) +
  
  # Exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 5, linetype = "dashed") +
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  
  scale_x_continuous(name = "Level", limits = c(1, 5), breaks = seq(1, 5, 1)) +  
  scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  
  theme_bw() +
  
  theme(legend.title = element_blank()) 
  
ggsave(plot = plot_1D, dpi = 600, filename = "plot_1D.png")

# variable importance
plot_imp_1D <- mgraph(importance_example$imp, graph = "IMP", leg = c("x1", "y"), col = "gray")
ggsave(plot = plot_imp_1D, dpi = 600, filename = "plot_imp_1D.png")

# prove baseline vector.
DMU <- 13

importance_bl <- Importance(
  M = m,
  RealL = 5, # Levels
  data = train_data, # data
  method = methods_SA,
  measure = measures_SA,
  #sampling = regular,
  baseline = train_data[DMU,], # mean, median, with the baseline example (should have the same attribute names as data).
  responses = TRUE
)

# input
data_1 <- round(importance_bl$sresponses[[2]]$y, 2)[,1]
data_4 <- round(importance_bl$sresponses[[2]]$y, 2)[,1]
data_13 <- round(importance_bl$sresponses[[2]]$y, 2)[,1]
data_prob_x1 <- rbind(data_1, data_4, data_13)
data_prob_x1 <- rbind(c(1,2,3,4,5), data_prob_x1)
data_prob_x1 <- as.data.frame(t(data_prob_x1))
names(data_prob_x1) <- c("level", "DMU 1", "DMU 4", "DMU 13")

data_long <- tidyr::pivot_longer(data_prob_x1, cols = c("DMU 1", "DMU 4", "DMU 13"), names_to = "variable", values_to = "value")

plot_1D_x1 <- ggplot(data_long, aes(x = level, y = value, color = variable)) +
  geom_line(size = 1.2) +
  
  # Exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 5, linetype = "dashed") +
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  
  scale_x_continuous(name = "Level", limits = c(1, 5), breaks = seq(1, 5, 1)) +  
  scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  
  theme_bw() +
  
  theme(legend.title = element_blank()) 
ggsave(plot = plot_1D_x1, dpi = 600, filename = "plot_1D_x1.png")

# output
# prove baseline vector.
DMU <- 4

importance_bl <- Importance(
  M = m,
  RealL = 5, # Levels
  data = train_data, # data
  method = methods_SA,
  measure = measures_SA,
  #sampling = regular,
  baseline = train_data[DMU,], # mean, median, with the baseline example (should have the same attribute names as data).
  responses = TRUE
)


data_1 <- round(importance_bl$sresponses[[3]]$y, 2)[,1]
data_4 <- round(importance_bl$sresponses[[3]]$y, 2)[,1]
data_13 <- round(importance_bl$sresponses[[3]]$y, 2)[,1]
data_prob_x1 <- rbind(data_1, data_4, data_13)
data_prob_x1 <- rbind(c(1,2,3,4,5), data_prob_x1)
data_prob_x1 <- as.data.frame(t(data_prob_x1))
names(data_prob_x1) <- c("level", "DMU 1", "DMU 4", "DMU 13")

data_long <- tidyr::pivot_longer(data_prob_x1, cols = c("DMU 1", "DMU 4", "DMU 13"), names_to = "variable", values_to = "value")

plot_1D_y <- ggplot(data_long, aes(x = level, y = value, color = variable)) +
  geom_line(size = 1.2) +
  
  # Exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 5, linetype = "dashed") +
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  
  scale_x_continuous(name = "Level", limits = c(1, 5), breaks = seq(1, 5, 1)) +  
  scale_y_continuous(name = "Probability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  
  theme_bw() +
  
  theme(legend.title = element_blank()) 
ggsave(plot = plot_1D_y, dpi = 600, filename = "plot_1D_y.png")

plot_imp_1D <- mgraph(importance_example$imp, graph = "IMP", leg = c("x1", "y"), col = "gray")
ggsave(plot = plot_imp_1D, dpi = 600, filename = "plot_imp_1D.png")