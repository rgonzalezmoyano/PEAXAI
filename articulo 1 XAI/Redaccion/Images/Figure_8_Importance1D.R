# visualization

load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/code paper/resultados_art_XAI_NN_CV_1_3.RData")
load("C:/Users/Ricardo/Documents/Doctorado EOMA/Cafee/articulo 1 XAI/code paper/resultados_art_XAI_NN_CV_1_3.RData")

# libraries
library(ggplot2)
library(tidyr)

data <- as.data.frame(t(list_method[["nnet"]][["result_SA"]]))
names(data) <- "importance"
row.names(data)

data_long <- data.frame(
  variable = c(row.names(data))
  )

data <- cbind(data_long, data$importance)

names(data) <- c("variable", "importance")

# plot 1D
plot <- ggplot(data = data, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(
    stat = "identity",
    fill = "white",
    color = "black"
  ) + 
  coord_flip() +
  labs(x = "variables",
       y = "relative importance",
       title = "SA relative importance ranking") +
  geom_text(aes(x = reorder(variable, importance), y = 0.08, label = variable), 
            hjust = -0.2, color = "black", size = 3.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Añadir una línea vertical en x=0
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # Fondo del panel blanco
        plot.background = element_rect(fill = "white", color = NA)) 

plot 
ggsave(plot = plot, dpi = 600, filename = "SA-1D relative importance.png")
