# visualization


# libraries
library(ggplot2)
library(tidyr)

# svm
imp_svm <- result_SA

data <- imp_svm
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
  labs(x = "variables",
       y = "relative importance",
       title = "Bar plot with the 1D input importances") +
  geom_text(aes(x = reorder(variable, importance), y = 0.08, label = variable), 
            hjust = -0.2, color = "black", size = 3.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Añadir una línea vertical en x=0
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),  # Fondo del panel blanco
        plot.background = element_rect(fill = "white", color = NA)) 

plot 
ggsave(plot = plot, dpi = 600, filename = "1D bar plot_example.png")


# correlation matrix


# Calcula la matriz de correlación
cor_data = cor(data)

# Gráfico de la matriz de correlación
ggcorrplot(cor_matrix, 
           method = "circle",  # Usa círculos para representar la correlación
           type = "lower",     # Solo muestra la mitad inferior de la matriz
           lab = TRUE,         # Muestra los valores de correlación
           lab_size = 3,       # Tamaño de los números
           colors = c("red", "white", "blue"),  # Colores para correlación negativa, 0 y positiva
           title = "Matriz de Correlación",
           ggtheme = theme_minimal())




L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
mgraph(L,graph="IMP",leg=names(sa_ssin),col="gray",Grid=10)
mgraph(L,graph="VEC",xval=1,Grid=10,data=sa_ssin,
       main="VEC curve for x1 influence on y") # or:
vecplot(I,xval=1,Grid=10,data=sa_ssin,datacol="gray",
        main="VEC curve for x1 influence on y") # same graph
vecplot(I,xval=c(1,2,3),pch=c(1,2,3),Grid=10,
        leg=list(pos="bottomright",leg=c("x1","x2","x3"))) # all x1, x2 and x3 VEC curves
           