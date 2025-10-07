### descriptive analysis

summary(data)

table(data$Region)

result <- as.data.frame(
  matrix(
    data = NA,
    ncol = 17,
    nrow = length(unique(data$Region))
  
))

names(data)

names(result) <- c("Region", "PVSCIE_m", "PVMATH_m", "PVREAD_m", "ESCS_m2", "TSRATIO", "EDUSHORT2", "PVSCIE_m", "PVMATH_m", "PVREAD_m", "ESCS_m2", "TSRATIO", "EDUSHORT2", "Samples", "SCHLTYPE1", "SCHLTYPE2", "SCHLTYPE3")

result$Region <- sort(unique(data$Region))

for (i in sort(unique(data$Region))) {
  
  data_i <- data %>% 
    filter(Region == i)

  avg <- colMeans(data_i[, c(x, y)])

  std_dev <- sapply(data_i[, c(x, y)], sd, na.rm = TRUE)
  
  samples <- nrow(data_i)
  
  schtype <- table(data_i$SCHLTYPE)
  
  # paste average
  result[i, 2:7] <- round(avg, 3)
  result[i, 8:13] <- round(std_dev, 3)
  result[i, 14] <- samples
  result[i, 15] <- schtype[1]
  result[i, 16] <- schtype[2]
  result[i, 17] <- schtype[3]
  
}

library(openxlsx)
write.xlsx(result, "data_descriptive.xlsx")
