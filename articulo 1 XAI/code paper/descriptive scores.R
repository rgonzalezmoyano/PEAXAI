### Table descriptive

# data from script

# x and y indexes
x <- c(10, 7, 6)
y <- c(3:5)
z <- c(2, 8) # environment variables

scores <- information_region[[1]]
summary(scores)

DMU <- as.data.frame(
  matrix(
    data = NA,
    ncol = 1,
    nrow = nrow(scores)
  )
)

DMU[1] <- 1:999

names(DMU) <- "DMU"

bcc_scores_out <- rad_out (
  tech_xmat = as.matrix(data[, x]),
  tech_ymat = as.matrix(data[, y]),
  eval_xmat = as.matrix(data[, x]),
  eval_ymat = as.matrix(data[, y]),
  convexity = TRUE,
  returns = "variable"
) 

DEA <- round(bcc_scores_out, 3)
data_scores <- cbind(DMU, DEA, scores)
summary(data_scores)
names(data_scores)

library(tidyr)

data_scores_long <- data_scores %>%
  pivot_longer(cols = c(svmPoly, nnet, DEA), names_to = "method", values_to = "score")



library(ggplot2)

plot <- ggplot(data_scores_long, aes(x = score, color = method)) +
  geom_density(trim = TRUE) +
  xlab("Kernel") +
  
  geom_hline(yintercept = 0)+
  # geom_vline(xintercept = 1, linetype = "dashed") +
  
  theme_bw() +
  labs(color = NULL) +
  theme(legend.position = "bottom")
  

plot

ggsave(plot = plot, dpi = 600, filename = "kernel_DEA.png")

# comparation efficiency DMUs

library(Benchmarking)

sDEA <- sdea (
  X = as.matrix(data[, x]),
  Y = as.matrix(data[, y]),
  RTS = "vrs",
  ORIENTATION = "out"
)$eff
min(sDEA)
sDEA <- round(sDEA, 3)

data_scores <- cbind(data_scores, sDEA)

data_scores$sDEA <- ifelse(data_scores$sDEA == "-Inf", NA, data_scores$sDEA)
colSums(is.na(data_scores))


names(data_scores) <- c("DMU", "DEA", "SVM", "NN",  "sDEA")

data_scores_long_super <- data_scores %>%
  pivot_longer(cols = c(sDEA, SVM, NN), names_to = "method", values_to = "score")

data_scores_long_super$method <- factor(data_scores_long_super$method, levels = c("sDEA", "SVM", "NN"))

# Crear el gráfico
plot <- ggplot(data_scores_long_super, aes(x = score, color = method)) +
  geom_density(trim = TRUE) +
  xlab("Kernel") +
  
  geom_hline(yintercept = 0)+
  # geom_vline(xintercept = 1, linetype = "dashed") +
  
  theme_bw() +
  labs(color = NULL) +
  theme(legend.position = "bottom") 


plot

ggsave(plot = plot, dpi = 600, filename = "kernel_sDEA.png")


### correlation
round(cor(na.omit(data_scores[3:5])), 3)


ggplot() +
  geom_point(data = data_scores[DEA == 1,], aes(x = DMU, y = svmPoly)) +
  geom_abline(intercept = 0, slope = 1) +
  
  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")
  
ggplot() +
  geom_point(data = data_scores[DEA == 1,], aes(x = DMU, y = nnet)) +
  geom_abline(intercept = 0, slope = 1) +
  
  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

ggplot() +
  geom_point(data = data_scores[DEA == 1,], aes(x = DMU, y = sDEA)) +
  geom_abline(intercept = 0, slope = 1) +
  
  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +

  theme_bw() +
  
  theme(legend.position = "bottom")


grafico <- data_scores[DEA == 1,]
grafico$id <- 1:nrow(grafico)

library(writexl)

# Exportar el data.frame a un archivo Excel
write_xlsx(grafico, path = "scores_efficient.xlsx")

plot <- ggplot() +
  geom_point(data = grafico, aes(x = DMU, y = sDEA)) +

  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "sDEA.png")


plot <- ggplot() +
  geom_point(data = grafico, aes(x = DMU, y = nnet)) +
  
  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "nnet.png")

plot <- ggplot() +
  geom_point(data = grafico, aes(x = DMU, y = svmPoly)) +
  
  # exes
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plot

ggsave(plot = plot, dpi = 600, filename = "SVM.png")
