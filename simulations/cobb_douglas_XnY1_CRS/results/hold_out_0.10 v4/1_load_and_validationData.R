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
  
  # technique & hyperparameters
  technique_cafee_DEA = rep(NA, repl),
  hyperparameters_cafee_DEA = rep(NA, repl),
  cut_off_cafee_DEA = rep(NA, repl),
  
  technique_cafee_BDEA = rep(NA, repl),
  hyperparameters_cafee_BDEA = rep(NA, repl),
  cut_off_cafee_BDEA = rep(NA, repl),
  
  # ==================== #
  # correlations pearson #
  # ==================== #
  corr_pearson_yD_DEA = rep(NA, repl),
  corr_pearson_yD_BDEA = rep(NA, repl),
  corr_pearson_yD_cross_eff = rep(NA, repl),
  corr_pearson_yD_super_eff = rep(NA, repl),
  corr_pearson_yD_cafee_DEA_50 = rep(NA, repl),
  corr_pearson_yD_cafee_DEA_cut_off = rep(NA, repl),
  corr_pearson_yD_cafee_BDEA_50 = rep(NA, repl),
  corr_pearson_yD_cafee_BDEA_cut_off = rep(NA, repl),
  corr_pearson_yD_p_cafee_DEA = rep(NA, repl),
  corr_pearson_yD_p_cafee_BDEA = rep(NA, repl),
  
  
  # ===================== #
  # correlations spearman #
  # ===================== #
  corr_spearman_yD_DEA = rep(NA, repl),
  corr_spearman_yD_BDEA = rep(NA, repl),
  corr_spearman_yD_cross_eff = rep(NA, repl),
  corr_spearman_yD_super_eff = rep(NA, repl),
  corr_spearman_yD_cafee_DEA_50 = rep(NA, repl),
  corr_spearman_yD_cafee_DEA_cut_off = rep(NA, repl),
  corr_spearman_yD_cafee_BDEA_50 = rep(NA, repl),
  corr_spearman_yD_cafee_BDEA_cut_off = rep(NA, repl),
  corr_spearman_yD_p_cafee_DEA = rep(NA, repl),
  corr_spearman_yD_p_cafee_BDEA = rep(NA, repl),
  
  # ==================== #
  # correlations kendall #
  # ==================== #
  corr_kendall_yD_DEA = rep(NA, repl),
  corr_kendall_yD_BDEA = rep(NA, repl),
  corr_kendall_yD_cross_eff = rep(NA, repl),
  corr_kendall_yD_super_eff = rep(NA, repl),
  corr_kendall_yD_cafee_DEA_50 = rep(NA, repl),
  corr_kendall_yD_cafee_DEA_cut_off = rep(NA, repl),
  corr_kendall_yD_cafee_BDEA_50 = rep(NA, repl),
  corr_kendall_yD_cafee_BDEA_cut_off = rep(NA, repl),
  corr_kendall_yD_p_cafee_DEA = rep(NA, repl),
  corr_kendall_yD_p_cafee_BDEA = rep(NA, repl),
  
  # mse
  mse_DEA = rep(NA, repl),
  mse_BDEA = rep(NA, repl),
  mse_cross_eff = rep(NA, repl),
  mse_super_eff = rep(NA, repl),
  mse_cafee_DEA_50 = rep(NA, repl),
  mse_cafee_DEA_cut_off = rep(NA, repl),
  mse_cafee_BDEA_50 = rep(NA, repl),
  mse_cafee_BDEA_cut_off = rep(NA, repl),
  
  # bias
  bias_DEA = rep(NA, repl),
  bias_BDEA = rep(NA, repl),
  bias_cross_eff = rep(NA, repl),
  bias_super_eff = rep(NA, repl),
  bias_cafee_DEA_50 = rep(NA, repl),
  bias_cafee_DEA_cut_off = rep(NA, repl),
  bias_cafee_BDEA_50 = rep(NA, repl),
  bias_cafee_BDEA_cut_off = rep(NA, repl),
  
  # dataset error NA
  num_error_cafee_DEA_50 = rep(0, repl),
  num_error_cafee_DEA_cut_off = rep(0, repl),
  num_error_cafee_BDEA_50 = rep(0, repl),
  num_error_cafee_BDEA_cut_off = rep(0, repl),
  
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
  data[i, 1:4] <- simulaciones[i, 2:5]
  
  # hyperparameters information
  data[i, 5:10] <- simulaciones[i, c(6:11)]
  
  # how manny NA_scenarios are there
  # dataset error
  data[i, 57] <- sum(is.na(simulaciones$corr_yD_cafee_DEA_50))
  data[i, 58] <- sum(is.na(simulaciones$corr_yD_cafee_DEA_cut_off))
  data[i, 59] <- sum(is.na(simulaciones$corr_yD_cafee_BDEA_50))
  data[i, 60] <- sum(is.na(simulaciones$corr_yD_cafee_BDEA_cut_off))
  
  simulaciones <- simulaciones %>%
    mutate_at( 
      c("mse_DEA", "mse_BDEA", "mse_cafee_DEA_50", "mse_cafee_DEA_cut_off", "mse_cafee_BDEA_50", "mse_cafee_BDEA_cut_off"),
      ~ifelse(. > 1000, NA, .)
    )
  
  # data error in mse
  # NA by cafee
  error_cafee <- length(which(apply(simulaciones[, c("mse_DEA", "mse_BDEA", "mse_cafee_DEA_50", "mse_cafee_DEA_cut_off", "mse_cafee_BDEA_50", "mse_cafee_BDEA_cut_off")], 1, function(row) any(is.na(row)))))
  
  # error total
  error_total <- data[i, 57] + data[i, 58] + data[i, 59] + data[i, 60]
  
  # NA by data
  error_data <- error_cafee - error_total
  data[i, 61] <- error_data
  
  # filter Na scenarios
  NA_values <- any(is.na(simulaciones))
  
  if (NA_values == TRUE) {
    
    NA_row <- which(rowSums(is.na(simulaciones)) > 0)
      
    simulaciones <- simulaciones[complete.cases(simulaciones), ]
    
  }
  
  # correlation person
  data[i, 11] <- mean(simulaciones$corr_pearson_yD_DEA)
  data[i, 12] <- mean(simulaciones$corr_pearson_yD_BDEA)
  data[i, 13] <- mean(simulaciones$corr_pearson_yD_cross_eff)
  data[i, 14] <- mean(simulaciones$corr_pearson_yD_super_eff)
  data[i, 15] <- mean(simulaciones$corr_pearson_yD_score_cafee_DEA_50)
  data[i, 16] <- mean(simulaciones$corr_pearson_yD_score_cafee_DEA_cut_off)
  data[i, 17] <- mean(simulaciones$corr_pearson_yD_score_cafee_BDEA_50)
  data[i, 18] <- mean(simulaciones$corr_pearson_yD_score_cafee_BDEA_cut_off)
  data[i, 19] <- mean(simulaciones$corr_pearson_yD_p_cafee_DEA)
  data[i, 20] <- mean(simulaciones$corr_pearson_yD_p_cafee_BDEA)

  # correlation sperman
  data[i, 21] <- mean(simulaciones$corr_spearman_yD_DEA)
  data[i, 22] <- mean(simulaciones$corr_spearman_yD_BDEA)
  data[i, 23] <- mean(simulaciones$corr_spearman_yD_cross_eff)
  data[i, 24] <- mean(simulaciones$corr_spearman_yD_super_eff)
  data[i, 25] <- mean(simulaciones$corr_spearman_yD_score_cafee_DEA_50)
  data[i, 26] <- mean(simulaciones$corr_spearman_yD_score_cafee_DEA_cut_off)
  data[i, 27] <- mean(simulaciones$corr_spearman_yD_score_cafee_BDEA_50)
  data[i, 28] <- mean(simulaciones$corr_spearman_yD_score_cafee_BDEA_cut_off)
  data[i, 29] <- mean(simulaciones$corr_spearman_yD_p_cafee_DEA)
  data[i, 30] <- mean(simulaciones$corr_spearman_yD_p_cafee_BDEA)

  # correlation kedall
  data[i, 31] <- mean(simulaciones$corr_kendall_yD_DEA)
  data[i, 32] <- mean(simulaciones$corr_kendall_yD_BDEA)
  data[i, 33] <- mean(simulaciones$corr_kendall_yD_cross_eff)
  data[i, 34] <- mean(simulaciones$corr_kendall_yD_super_eff)
  data[i, 35] <- mean(simulaciones$corr_kendall_yD_score_cafee_DEA_50)
  data[i, 36] <- mean(simulaciones$corr_kendall_yD_score_cafee_DEA_cut_off)
  data[i, 37] <- mean(simulaciones$corr_kendall_yD_score_cafee_BDEA_50)
  data[i, 38] <- mean(simulaciones$corr_kendall_yD_score_cafee_BDEA_cut_off)
  data[i, 39] <- mean(simulaciones$corr_kendall_yD_p_cafee_DEA)
  data[i, 40] <- mean(simulaciones$corr_kendall_yD_p_cafee_BDEA)

  # mse
  data[i, 41] <- mean(simulaciones$mse_DEA)
  data[i, 42] <- mean(simulaciones$mse_BDEA)
  data[i, 43] <- mean(simulaciones$mse_cross_eff)
  data[i, 44] <- mean(simulaciones$mse_super_eff)
  data[i, 45] <- mean(simulaciones$mse_cafee_DEA_50)
  data[i, 46] <- mean(simulaciones$mse_cafee_DEA_cut_off)
  data[i, 47] <- mean(simulaciones$mse_cafee_BDEA_50)
  data[i, 48] <- mean(simulaciones$mse_cafee_BDEA_cut_off)

  # bias 
  data[i, 49] <- mean(abs(simulaciones$bias_DEA))
  data[i, 50] <- mean(abs(simulaciones$bias_BDEA))
  data[i, 51] <- mean(abs(simulaciones$bias_cross_eff))
  data[i, 52] <- mean(abs(simulaciones$bias_super_eff))
  data[i, 53] <- mean(abs(simulaciones$bias_cafee_DEA_50))
  data[i, 54] <- mean(abs(simulaciones$bias_cafee_DEA_cut_off))
  data[i, 55] <- mean(abs(simulaciones$bias_cafee_BDEA_50))
  data[i, 56] <- mean(abs(simulaciones$bias_cafee_BDEA_cut_off))

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
