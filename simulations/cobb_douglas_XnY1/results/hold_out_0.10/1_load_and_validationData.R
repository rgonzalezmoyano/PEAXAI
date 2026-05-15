library(dplyr)

# Obtener la lista de archivos en el directorio
archivos <- list.files()

archivos <- archivos[grep("^cobb", archivos)]

repl <- length(archivos)

data <- data.frame (
  # general
  DGP = rep(NA, repl),
  scenario = rep(NA, repl),
  N = rep(NA, repl),
  noise = rep(NA, repl),
  technique = rep(NA, repl),
  
  # correlations
  corr_yD_DEA = rep(NA, repl),
  corr_yD_BDEA = rep(NA, repl),
  corr_yD_cafee_DEA = rep(NA, repl),
  corr_yD_cafee_BDEA = rep(NA, repl),
  
  # mse
  mse_DEA = rep(NA, repl),
  mse_BDEA = rep(NA, repl),
  mse_cafee_DEA = rep(NA, repl),
  mse_cafee_BDEA = rep(NA, repl),
  
  # bias
  bias_DEA = rep(NA, repl),
  bias_BDEA = rep(NA, repl),
  bias_cafee_DEA = rep(NA, repl),
  bias_cafee_BDEA = rep(NA, repl),
  
  # dataset error NA
  num_error_cafee_DEA = rep(0, repl),
  num_error_cafee_BDEA = rep(0, repl),

  # Error data
  num_error_DEA = rep(0, repl)
)

# Iterar sobre cada archivo
for (i in 1:length(archivos)) {
  
  # Cargar el archivo y asignarlo al entorno de trabajo con su nombre de archivo como variable
  load(archivos[i])
  
  # PreProcess
  # Comprobate N repl
  if (nrow(simulaciones) != 100) {
    print(paste("Error en i ==", i))
  } 
  
  # Common information
  data[i, 1:5] <- simulaciones[i, 2:6]
  
  # how manny NA_scenarios are there
  # dataset error
  data[i, 18] <- sum(is.na(simulaciones$corr_yD_cafee_DEA))
  data[i, 19] <- sum(is.na(simulaciones$corr_yD_cafee_BDEA))
  
  simulaciones <- simulaciones %>%
    mutate_at(c("mse_DEA", "mse_BDEA", "mse_cafee_DEA", "mse_cafee_BDEA"), ~ifelse(. > 1000, NA, .))
  
  # data error in mse
  # NA by cafee
  error_cafee <- length(which(apply(simulaciones[, c("mse_DEA", "mse_BDEA", "mse_cafee_DEA", "mse_cafee_BDEA")], 1, function(row) any(is.na(row)))))
  
  # error total
  error_total <- data[i, 18] + data[i, 19]
  
  # NA by data
  error_data <- error_cafee - error_total
  data[i, 20] <- error_data
  
  
  
  # filter Na scenarios
  NA_values <- any(is.na(simulaciones))
  
  if (NA_values == TRUE) {
    
    NA_row <- which(rowSums(is.na(simulaciones)) > 0)
      
    simulaciones <- simulaciones[complete.cases(simulaciones), ]
    
  }
  
  # correlation
  data[i, 6] <- mean(simulaciones$corr_yD_DEA)
  data[i, 7] <- mean(simulaciones$corr_yD_BDEA)
  data[i, 8] <- mean(simulaciones$corr_yD_cafee_DEA)
  data[i, 9] <- mean(simulaciones$corr_yD_cafee_BDEA)
    
  # mse
  data[i, 10] <- mean(simulaciones$mse_DEA)
  data[i, 11] <- mean(simulaciones$mse_BDEA)
  data[i, 12] <- mean(simulaciones$mse_cafee_DEA)
  data[i, 13] <- mean(simulaciones$mse_cafee_BDEA)
    
  # bias 
  data[i, 14] <- mean(simulaciones$bias_DEA)
  data[i, 15] <- mean(simulaciones$bias_BDEA)
  data[i, 16] <- mean(simulaciones$bias_cafee_DEA)
  data[i, 17] <- mean(simulaciones$bias_cafee_BDEA)
  
}

data_by_N <- data %>% 
  group_by(N) %>% 
  summarize(corr_yD_DEA = mean(corr_yD_DEA),
            corr_yD_BDEA = mean(corr_yD_BDEA),
            corr_yD_cafee_DEA = mean(corr_yD_cafee_DEA),
            corr_yD_cafee_BDEA = mean(corr_yD_cafee_BDEA),
            mse_DEA = mean(mse_DEA),
            mse_BDEA = mean(mse_BDEA),
            mse_cafee_DEA = mean(mse_cafee_DEA),
            mse_cafee_BDEA = mean(mse_cafee_BDEA),
            bias_DEA =  mean(bias_DEA),
            bias_BDEA =  mean(bias_BDEA),
            bias_cafee_DEA =  mean(bias_cafee_DEA),
            bias_cafee_BDEA =  mean(bias_cafee_BDEA)
  )

data_by_scenario <- data %>% 
  group_by(scenario) %>% 
  summarize(corr_yD_DEA = mean(corr_yD_DEA),
            corr_yD_BDEA = mean(corr_yD_BDEA),
            corr_yD_cafee_DEA = mean(corr_yD_cafee_DEA),
            corr_yD_cafee_BDEA = mean(corr_yD_cafee_BDEA),
            mse_DEA = mean(mse_DEA),
            mse_BDEA = mean(mse_BDEA),
            mse_cafee_DEA = mean(mse_cafee_DEA),
            mse_cafee_BDEA = mean(mse_cafee_BDEA),
            bias_DEA =  mean(bias_DEA),
            bias_BDEA =  mean(bias_BDEA),
            bias_cafee_DEA =  mean(bias_cafee_DEA),
            bias_cafee_BDEA =  mean(bias_cafee_BDEA)
  )

data_by_noise <- data %>% 
  group_by(noise) %>% 
  summarize(corr_yD_DEA = mean(corr_yD_DEA),
            corr_yD_BDEA = mean(corr_yD_BDEA),
            corr_yD_cafee_DEA = mean(corr_yD_cafee_DEA),
            corr_yD_cafee_BDEA = mean(corr_yD_cafee_BDEA),
            mse_DEA = mean(mse_DEA),
            mse_BDEA = mean(mse_BDEA),
            mse_cafee_DEA = mean(mse_cafee_DEA),
            mse_cafee_BDEA = mean(mse_cafee_BDEA),
            bias_DEA =  mean(bias_DEA),
            bias_BDEA =  mean(bias_BDEA),
            bias_cafee_DEA =  mean(bias_cafee_DEA),
            bias_cafee_BDEA =  mean(bias_cafee_BDEA)
  )



library("openxlsx")
#install.packages("openxlsx")

# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Agregar las hojas al libro
addWorksheet(wb, sheetName = "data")
writeData(wb, sheet = "data", x = data)

addWorksheet(wb, sheetName = "by_scenario")
writeData(wb, sheet = "by_scenario", x = data_by_scenario)

addWorksheet(wb, sheetName = "by_noise")
writeData(wb, sheet = "by_noise", x = data_by_noise)

addWorksheet(wb, sheetName = "by_N")
writeData(wb, sheet = "by_N", x = data_by_N)

saveWorkbook(wb, file = "recopilacion_datos.xlsx")
