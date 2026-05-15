# Obtener la lista de archivos en el directorio
archivos <- list.files()

archivos <- archivos[grep("^cobb", archivos)]

repl <- 47

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
  bias_cafee_BDEA = rep(NA, repl)
  
)

# Iterar sobre cada archivo
for (i in 1:length(archivos)) {
    
    # Cargar el archivo y asignarlo al entorno de trabajo con su nombre de archivo como variable
    load(archivos[i])

    data[i, 1:5] <- simulaciones[i, 2:6]
    
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

<<<<<<< HEAD
library("openxlsx")
#install.packages("openxlsx")
write.xlsx(data, file = "recopilacion_datos.xlsx")
=======
install.packages("openxlsx")
install.packages("Rtools")
library(open)
>>>>>>> 74da1cd152472c0d27d3468faaaf8caaeaa4d3a1
