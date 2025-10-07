# need to load "new_dataset_toy_lebeled.Rdata"

# load cafee
devtools::load_all()
# libraries 
library(ggplot2)


x <- 1
y <- 2

plot <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) + 
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot

#ggsave(plot = plot, dpi = 600, filename = "DEA_add_label.png")
compute_scores_additive(data, x = x, y = y)


data_eff <- data[data$class_efficiency == "efficient",]
compute_scores_additive(data_eff, x = x, y = y)

# determinate eff units
n_eff <- nrow(data_eff)

# select unit to compare
final_data <- data

prop_eff <- prop.table(table(final_data$class_efficiency))[1]
cut_off <- 0.75

new_eff_point <- as.data.frame(matrix(
  data = NA,
  ncol = ncol(data[, c(x,y)]),
  nrow = 0
))

names(new_eff_point) <- names(data[, c(x,y)])

ineff_conexion <- as.data.frame(matrix(
  data = NA,
  ncol = 2,
  nrow = 0
))

names(ineff_conexion) <- c("reference", "unit_conexion")

while (prop_eff < cut_off) {
  
  repeat {
    
    # select unit reference
    reference <- sample(n_eff, 1)
    
    # units to copare
    conexion <- data_eff[-reference,]
    
    unit_conexion <- sample(nrow(conexion), 1)
    
    conexion_test <- data.frame(
      reference = reference,
      unit_conexion = unit_conexion
    )
    
    bor <- ineff_conexion[reference == reference, ] 
    bor[unit_conexion == unit_conexion]
    
    is_in_ineff_conexion <- any(apply(ineff_conexion, 1, function(row) all(row == unlist(conexion_test))))
    
    
    if (is_in_ineff_conexion == FALSE) {
      break
    }
    
  }
 
  delta <- runif(1, min = 0, max = 1)
  
  # generate SMOTE unit
  new_point <- data_eff[reference, c(x,y)] + (data_eff[unit_conexion, c(x,y)] - data_eff[reference, c(x,y)]) * delta
  
  test_eff <- rbind(data_eff[, c(x,y)], new_point)
  idx_new_point <- n_eff + 1
  
  test_additive <- compute_scores_additive(test_eff, x = x, y = y)
  
  if (test_additive[idx_new_point] > 0) {
    
    # this conexion makes inefficient units
    conexion_ineff <- data.frame(
      reference = reference,
      unit_conexion = unit_conexion
    )
    
    conexion_ineff2 <- data.frame(
      reference = conexion_ineff$unit_conexion,
      unit_conexion = conexion_ineff$reference
    )
    
    ineff_conexion <- rbind(ineff_conexion, conexion_ineff, conexion_ineff2)
    
  } else {
    
    # add correct unit efficient
    new_eff_point <- rbind(new_eff_point, new_point)
    
    new_point$class_efficiency <- "efficient"
    final_data <- rbind(final_data, new_point)
    
  }
  
  prop_eff <- prop.table(table(final_data$class_efficiency))[1]
  
}

plot <- ggplot() +
  geom_point(data = final_data, aes(x = x1, y = y, color = class_efficiency)) +
  scale_color_manual(values = c("green4", "red"), name = "Class", labels = c("efficient", "inefficient")) +
  labs(x = "input", y = "output") +
  
  # name DMUs
  geom_text(data = data[data$class_efficiency == "efficient", ],
            aes(x = x1, y = y, label = row.names(data[data$class_efficiency == "efficient", ])),
            vjust = -1, hjust = 1) + 
  
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_bw() +
  theme(legend.position = "bottom")

plot
ggsave(plot = plot, dpi = 600, filename = "DEA_add_label.png")
