# visualization
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/code paper/resultados_art_XAI_NN_CV_1_3.RData")

# libraries
library(ggplot2)
library(tidyr)
library(tools)

# svm
imp <- list_method[["nnet"]][["result_SA"]]

names(imp)[4] <- "personnel expenses"

colnames(imp) <- gsub("_", " ", colnames(imp))

colnames(imp) <- sub("^(.)", "\\U\\1", tolower(colnames(imp)), perl = TRUE)

data <- imp
df_long <- gather(data, key = "variable", value = "importance")
df_long$importance <- as.numeric(df_long$importance)

# plot 1D
plot <- ggplot(data = df_long, aes(x = reorder(variable, importance), y = importance)) +
  
  geom_bar(
    stat = "identity",
    fill = "white",
    color = "black"
  ) + 
  
  coord_flip() +
  
  labs(x = "Variables",
       y = "Relative importance") +
  
  # geom_text(aes(x = reorder(variable, importance), y = 0.08, label = variable), 
  #           hjust = -0.5, color = "black", size = 3.5) +
  
  geom_text(aes(label = paste0(variable, " ", round(importance * 100, 1), "%"), 
                y = ifelse(importance == max(importance), importance - 0.07, importance + 0.02)),  
            hjust = ifelse(df_long$importance == max(df_long$importance), 0.5, 0),  
            color = "black", size = 4.5) +
  
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Añadir una línea vertical en x=0
  
  #scale_x_continuous(limits = c(0, 0.6)) +
  
  theme_minimal() +
  
  theme(axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # Fondo del panel blanco
        plot.background = element_rect(fill = "white", color = NA)) 

plot <- ggplot(data = df_long, aes(x = reorder(variable, importance), y = importance)) +
  
  geom_bar(
    stat = "identity",
    fill = "white",
    color = "black"
  ) + 
  
  coord_flip() +
  
  labs(x = "Variables",
       y = "Relative importance") +
  
  geom_text(aes(label = paste0(variable, " ", round(importance * 100, 1), "%"), 
                y = ifelse(importance == max(importance), importance - 0.07, importance + 0.02)),  
            hjust = ifelse(df_long$importance == max(df_long$importance), 0.5, 0),  
            color = "black", size = 4.5) +
  
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  
  theme_minimal() +
  
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14),       # Tamaño del texto del eje X
    axis.title.x = element_text(size = 16),      # Tamaño del título del eje X
    axis.title.y = element_text(size = 16),      # Tamaño del título del eje Y
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

plot 
ggsave(plot = plot, dpi = 600, width = 10, heigh = 6, filename = "1D bar plot_CV_importance.png")

