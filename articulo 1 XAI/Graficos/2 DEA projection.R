### Generate DEA projection

devtools::load_all()
set.seed(1)
data <- reffcy (
  DGP = "cobb_douglas_XnY1",
  parms = list (
    N = 30,
    nX = 1
  )
)

x <- 1
y <- 2

scores <- rad_out (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = TRUE,
  returns = "variable"
) 

data <- cbind(data, scores)

eff_dmu <- which(data$scores < 1.00001)

grph_data <- data[eff_dmu, ]
grph_data <- grph_data[order(grph_data$x1, grph_data$y), ]

extremos <- as.data.frame(
  matrix(
    data = NA, ncol = 4, nrow = 2
    )
  )

extremos[1, ] <- data.frame(x1 = grph_data$x1[1], y = 0, yD = 0, scores = 0)
extremos[2, ] <- data.frame(x1 = 10, y = grph_data$y[7], yD = 0, scores = 0)

names(extremos) <- names(data)
grph_data <- rbind(grph_data, extremos)


grph_data <- grph_data[order(grph_data$x1, grph_data$y), ]

# projection 
projection <- as.data.frame(
  matrix(
    data = NA, ncol = 2, nrow = 2
  )
)

projection[1, ] <- data.frame(x1 = data[3,1], y = data[3,2])
projection[2, ] <- data.frame(x1 = 5, y = 6.6)

names(projection) <- c("x1", "y")
library(ggplot2)
# plot
plot <- ggplot() +
  geom_point(data = data, aes(x = x1, y = y)) +

  geom_line(data = grph_data, aes(x = x1, y = y)) +
  
  #geom_line(data = projection, aes(x = x1, y = y)) +
  
  geom_segment(
    aes(
      x = projection$x1[1],
      y = projection$y[1], 
      xend = 5.07,
      yend = 6.5), 
    arrow = arrow(length = unit(0.2, "cm")), 
    color = "black", size = 0.5) +
  
  geom_point(data = projection[2, ], aes(x = x1, y = y), color = "yellow4", size = 3, ) +
  
  # names exes
  xlab("Input") +
  ylab("Output") +
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  theme_bw() +
  
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),  # Subido a 16
    axis.text = element_text(size = 14),   # Subido a 14
    plot.title = element_text(size = 18),  # Subido a 18
    legend.text = element_text(size = 14), # Subido a 14
    legend.title = element_text(size = 15) # Subido a 15
  )

plot

# ggsave(plot = plot, dpi = 600, filename = "DEA projection.png")
ggsave(file = "projection.png", plot = plot, dpi = 600, width = 10, heigh = 6)
