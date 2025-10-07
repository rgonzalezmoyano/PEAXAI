# Test convex with hull

library(cxhull)
library(ggplot2)
library(dplyr)

data <- as.data.frame(matrix(
  data = NA, 
  ncol = 2,
  nrow = 7
))

names(data) <- c("x", "y")


x <- 1
y <- 2

data$x <- c(2,3,6,8,11, 6, 10)
data$y <- c(1,4,9,11,12, 5, 6)

ggplot() +
  geom_point(data = data, aes(x = x, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

devtools::load_all()
compute_scores_additive(data = data, x = x, y = y)

hull <- cxhull(as.matrix(data))

facets <- matrix(
  data = NA,
  ncol = ncol(data),
  nrow = length(hull[["facets"]])
)  

facets_detect <- lapply(hull[["facets"]], function(facet) {
  
  all_facets <- facet[["vertices"]]
  
  # if (length(all_facets) == 5) {
  #   
  #   all_facets <- data_hull[all_facets, ]
  #   colSums(all_facets * lambda)
  #   
  # } else {
  #   
  #   NULL  
  #   
  # }
  
})

if (any(sapply(facets_detect, is.null))) {
  facets_detect <- facets_detect[!sapply(facets_detect, is.null)]
}

facets_center <- lapply(hull[["facets"]], function(facet) {
  
  all_facets <- facet[["center"]]
  
  # if (length(all_facets) == 5) {
  #   
  #   all_facets <- data_hull[all_facets, ]
  #   colSums(all_facets * lambda)
  #   
  # } else {
  #   
  #   NULL  
  #   
  # }
  
})

if (any(sapply(facets_center, is.null))) {
  facets_center <- facets_center[!sapply(facets_center, is.null)]
}

facets_center <-  do.call(rbind, facets_center) 
facets_center <- as.data.frame(facets_center)
names(facets_center) <- names(data)[c(x,y)]

facets <- do.call(rbind, facets_detect) 
facets <- as.data.frame(facets)

lines <- facets %>%
  rowwise() %>%
  mutate(
    x1 = data$x[V1],
    y1 = data$y[V1],
    x2 = data$x[V2],
    y2 = data$y[V2]
  ) %>%
  ungroup()

ggplot() +
  # Puntos del conjunto de datos original
  geom_point(data = data, aes(x = x, y = y)) +
  geom_point(data = facets_center, aes(x = x, y = y)) +
  
  # Líneas que conectan los puntos
  geom_segment(data = lines, aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue") +
  
  # Líneas de referencia
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  # Tema visual
  theme_bw() +
  theme(legend.position = "bottom")

# Función para identificar puntos Pareto eficientes
pareto_efficient <- function(data, x, y) {

  n <- nrow(data)
  input_cols <- names(data)[x]
  output_cols <- names(data)[y]
  is_efficient <- rep(TRUE, n) # Inicialmente, todos los puntos son eficientes
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        # Condiciones de dominancia: los inputs son menores o iguales, y los outputs son mayores o iguales
        dominates <- all(data[j, input_cols] <= data[i, input_cols]) &&
          all(data[j, output_cols] >= data[i, output_cols]) &&
          (any(data[j, input_cols] < data[i, input_cols]) ||
             any(data[j, output_cols] > data[i, output_cols]))
        
        if (dominates) {
          is_efficient[i] <- FALSE
          break
        }
      }
    }
  }
  
  return(is_efficient)
}
data <- rbind(data,facets_center)
data$efficient <- pareto_efficient(data, x, y)
efficient_points <- data[data$efficient, ]


ggplot() +
  geom_point(data = efficient_points, aes(x = x, y = y)) +
  # exes
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom")

compute_scores_additive(data = efficient_points, x = x, y = y)
devtools::load_all()




efficient_hull <- cxhull(as.matrix(efficient_points[, c(x,y)]))

efficient_facets <- matrix(
  data = NA,
  ncol = ncol(efficient_points[,c(x,y)]),
  nrow = nrow(efficient_points[,c(x,y)])
)  

lambda <- 1/ncol(data[,c(x,y)])

facets_detect <- lapply(efficient_hull[["facets"]], function(facet) {
  
  all_facets <- facet[["vertices"]]
  
  # if (length(all_facets) == ncol(data[,c(x,y)])) {
  # 
  #   all_facets <- efficient_points[all_facets, c(x,y)]
  #   colSums(all_facets * lambda)
  # 
  # } else {
  # 
  #   NULL
  # 
  # }
  
})

if (any(sapply(facets_detect, is.null))) {
  facets_detect <- facets_detect[!sapply(facets_detect, is.null)]
}

efficient_facets <- do.call(rbind, facets_detect) 

facets <- as.data.frame(efficient_facets)

lines <- facets %>%
  rowwise() %>%
  mutate(
    x1 = data$x[V1],
    y1 = data$y[V1],
    x2 = data$x[V2],
    y2 = data$y[V2]
  ) %>%
  ungroup()

ggplot() +
  # Puntos del conjunto de datos original
  geom_point(data = data, aes(x = x, y = y)) +
  
  # Líneas que conectan los puntos
  geom_segment(data = lines, aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue") +
  
  # Líneas de referencia
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  # Tema visual
  theme_bw() +
  theme(legend.position = "bottom")

facets_detect <- lapply(efficient_hull[["facets"]], function(facet) {
  
  all_facets <- facet[["vertices"]]

  if (length(all_facets) == ncol(data[,c(x,y)])) {

    all_facets <- efficient_points[all_facets, c(x,y)]
    colSums(all_facets * lambda)

  } else {

    NULL

  }
  
})

if (any(sapply(facets_detect, is.null))) {
  facets_detect <- facets_detect[!sapply(facets_detect, is.null)]
}

convx_points <- do.call(rbind, facets_detect) 

convx_points <- as.data.frame(convx_points)

ggplot() +
  # Puntos del conjunto de datos original
  geom_point(data = data, aes(x = x, y = y), color = "blue") +
  geom_point(data = convx_points, aes(x = x, y = y),  color = "red", ) +
  
  # Líneas que conectan los puntos
  geom_segment(data = lines, aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue") +
  
  
  # Líneas de referencia
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  # Tema visual
  theme_bw() +
  theme(legend.position = "bottom")

data_test <- rbind(data[data$efficient == TRUE, c(x,y)], convx_points)


data_test$efficient <-  pareto_efficient(data_test)
compute_scores_additive(data_test, x = x, y = y)

### libreria paret
#install.packages("rPref")
library(rPref)
data <- rbind(data,facets_center)
p <- high(y) * low(x)
efficient_rpref <- psel(data[,c(x,y)], p, top = nrow(data))
names(efficient_rpref)[3] <- "classEfficiency"
compute_scores_additive(efficient_rpref[, c(x,y)], x = x, y = y)



