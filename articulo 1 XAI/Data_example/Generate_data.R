### Example simulation

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

# change to be similar to DEA dataset examples
data[22, "y"] <- 6
data[21, "y"] <- 7.5

# 
# library(openxlsx)
# write.xlsx(data, "data_example.xlsx")

# x and y indexes
x <- 1
y <- 2
z <- NULL

# import

# different types to label
target_method <- "additive"

set.seed(314)
methods <- list (
  # svm
  "svmPoly" = list(
      hyparams = list(
        "degree" = c(1, 2, 3, 4, 5),
        "scale" = c(0.001, 0.1, 1, 10, 100),
        "C" = c(0.001, 0.1, 1, 10, 100)
      )
  )
)

# =========== #
# score cafee #
# =========== #    

# efficiency orientation
convexity <- TRUE
returns <- "variable"

# SMOTE proportions
balance_data <- c(0.4, 4)

# ML metric
metric = "F"

# scenarios to peer
scenarios <- seq(0.75, 0.95, 0.1)

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
  

# loop method
for (i in 1:length(methods)) {
  
  # for (a in 1:nrow(balance_data)) {
  #   
  # }
  # console information
  print(paste("METODO:", i,  names(methods)[i]))
  print("")
  
  # model result
  final_model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    #z = z,
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


################################################################################
# sintetic data example

data
idx_eff <- which(data$class_efficiency == "efficient")

# save distances structure
save_dist <- matrix(
  data = NA,
  ncol = length(idx_eff),
  nrow = length(idx_eff)
)

save_near <- matrix(
  data = NA,
  ncol = length(idx_eff),
  nrow = length(idx_eff)
)

pos_loop <- 0

for (unit_eff in idx_eff) {
  pos_loop <- pos_loop + 1
  # set reference
  reference <- data[unit_eff, c(x,y)]
  
  distance <- unname(apply(data[idx_eff, c(x,y)], 1, function(x) sqrt(sum((x - reference)^2))))
  
  # get position in save results
  idx_dis <- which(idx_eff == unit_eff)
  save_dist[idx_dis,] <- as.matrix(distance)
  
  # get k-near
  idx_distance <- which(sort(distance) == idx_eff)
  
  idx_two <- which(distance == distance_two[2] | distance == distance_two[3])
  
  save_near[pos_loop,] <- idx_eff[idx_two]
  
}

# delete not real near observations
for (ref in 1:length(idx_eff)) {
  
  unit_ref <- idx_eff[ref] # 13
  
  save_near[ref, ]
  
  unit_ref_1 <- save_near[ref, 1]
  pos_ref_1 <- which(idx_eff == unit_ref_1)
  near_ref_1  <- save_near[pos_ref_1, ]
  
  if (!(unit_ref %in% near_ref_1)) {
    
  }
  
  unit_ref_2 <- save_near[ref, 2]
  pos_ref_2 <- which(idx_eff == unit_ref_2)
  near_ref_2  <- save_near[pos_ref_2, ]
  
  if (!(unit_ref %in% near_ref_2)) {
    print("hola")
  }
  
}

################################################################################

### determinate efficient class
# fold 1_label_additive

### determinate efficient class BALANCED INPUT
data_gra <- data
data <- eval_data
efficient_data_0 <- data_gra[data_gra$class_efficiency == "efficient", ]

plot1 <- ggplot() +
  
  # original points
  geom_point(data = data, aes(x = x1, y = y), size = 1) +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = efficient_data_0, aes(x = x1, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = "green4", name = "Class") +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot1

ggsave(plot = plot1, dpi = 600, filename = "DEA_label_efficient.png")

### plot2 no efficient
inefficient_data_0 <- data_gra[data_gra$class_efficiency == "not_efficient", ]

plot2 <- ggplot() +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) +
  
  geom_point(data = inefficient_data_0, aes(x = x1, y = y, color = class_efficiency)) +
  
  scale_color_manual(values = "red", name = "Class", labels = "inefficient") +
  labs(x = "input", y = "output") +
  
  # original points
  geom_point(data = data, aes(x = x1, y = y), size = 1.3) +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot2

ggsave(plot = plot2, dpi = 600, filename = "DEA_label_inefficient.png")


### plot3
efficient_data <- rbind(efficient_data_0, new_dmu_values)


plot3 <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = "green4", name = "Class") +
  labs(x = "input", y = "output") +
  theme_bw() +
  theme(legend.position = "bottom")

plot3


ggsave(plot = plot3, dpi = 600, filename = "DEA_label3.png")

### plot4
plot4 <- ggplot() +
  geom_point(data = data_gra, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")


plot4


ggsave(plot = plot4, dpi = 600, filename = "DEA_label_all.png")

# ============= #
# Generate plot #
# ============= #

# make a grid of the predictors
grid <- expand.grid (
  x1 = seq(0, 10, length = 300),
  y = seq(0, 10, length = 300)
)

# grid <- expand.grid (
#   x1 = seq(1, 6, length = 150),
#   y = seq(1, 8, length = 150)
# )

grid$label <- predict(final_model$final_model, grid, type = "raw")

# i <- 2
# i <- c(12, 13, 15, 16, 17, 18, 23, 24)

# scores_r <- scores[scores$score_cafee == 1.095, ]
# r <- rownames(scores_r)
# filter_ggplot_data <- data[r,]
all_dmu <- data
data <- eval_data

data <- all_dmu
n <- 3
# draw projection
new <- data.frame(
  x1 = data[n,1],
  y = data[n,2] * scores_cafee[n],
  yD = data[n,3]
)

data <- rbind(data, new)

plot5 <- ggplot(data = data) +
  geom_point(data = grid, aes(x = x1, y = y, color = label), size = 0.6, alpha = 0.3) +
  geom_point(aes(x = x1, y = y)) +
  # scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
  scale_color_manual(values = c("olivedrab2", "pink"), name = "Class", labels = c("efficient", "inefficient")) +

  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[n,],
            aes(x = x1, y = y, label = row.names(data[n,])),
            vjust = -1, hjust = 1.5) +
  
  # name projection
  geom_text(data = data[31,],
            aes(x = x1, y = y, label = "3'"),
            vjust = -1, hjust = 1.5) +
  
  # projection segment
  geom_segment(x = data[n,1], y = data[n,2], xend = data[31,1], yend = data[31,2], linetype = "dashed") +
  
  # color of projection
  geom_point(data = data[31,], aes(x = x1, y = y), color = "yellow4", size = 2) +
  
  # exes
  xlim(0, 10) +
  ylim(0, 10) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

plot5


ggsave(plot = plot5, dpi = 600, filename = "SVM_balance_pro.png")

result_scores <- as.data.frame(
  matrix(
    data = NA,
    ncol = 3,
    nrow = nrow(data)
  )
)

names(result_scores) <- c("DMU", "DEA score", "SVM score")

result_scores$DMU <- 1:nrow(data)

result_scores$`DEA score` <- round(bcc_scores_out, 3)
result_scores$`SVM score` <- scores

cor(x= result_scores$`DEA score`, y = result_scores$`SVM score`, method = "pearson")

library(openxlsx)
write.xlsx(result_scores, "result_example.xlsx")
devtools::load_all(
  
)