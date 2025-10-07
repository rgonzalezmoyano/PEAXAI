# ============================================================================ #
# ========================= Betas comparation ================================ #
# ============================================================================ #


# ------------------------------------------------------------------------------
# Load libraries ---------------------------------------------------------------
#-------------------------------------------------------------------------------
library(readxl)
library(readxl)

# ------------------------------------------------------------------------------
# Get names --------------------------------------------------------------------
#-------------------------------------------------------------------------------
list_data <- list.files(
  path = "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez",
  pattern = "\\.xlsx$"
)

files <- c (
  "betas1.xlsx",
  "betas2.xlsx",
  "betas3.xlsx",
  "betas4.xlsx",
  "betas5.xlsx",
  "betas6.xlsx",
  "betas7.xlsx",
  "betas8.xlsx",
  "betas9.xlsx",
  "betas10.xlsx",
  "betas11.xlsx"
)

data <- data.frame (
  beta = NA,
  probability = NA
)

for(i in 1:length(files)) {

  dir  <- c("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/")
  df <- read_xlsx(paste0(dir, files[i]))
  df[, 2] <- max(df[, 2])
  
  data <- rbind(data, df)
}

data <- data[2:1068, ]
data[, 2] <- round(data[, 2], 2)

data2 <- data2 %>%
  filter(probability %in% c(0.75, 0.85, 0.95))

graph <- ggplot(data2) +
  geom_density(aes(x = beta, fill = factor(probability)), alpha = 0.4) +
  theme_bw() +
  labs(fill = "Threshold")

str(data)

ggsave(file = "img1.png", plot = graph, dpi = 600, width = 10, heigh = 6)
