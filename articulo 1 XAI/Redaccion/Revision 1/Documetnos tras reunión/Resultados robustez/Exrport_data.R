# ============================================================================ #
# ======================= Export results to Excel ============================ #
# ============================================================================ #


# ------------------------------------------------------------------------------
# Load libraries ---------------------------------------------------------------
#-------------------------------------------------------------------------------
library(openxlsx)
library(writexl)

# ------------------------------------------------------------------------------
# Load data --------------------------------------------------------------------
#-------------------------------------------------------------------------------
# load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_logit_g_mean.Rdata")
# load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_SVM_g_mean.RData")
load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_NN_g_x0_y0.RData")
# name_data <- "logit_g_x0_y0"
# name_data <- "SVM_g_x0_y0"
name_data <- "NN_g_x0_y0"

# ------------------------------------------------------------------------------
# Best balance rate ------------------------------------------------------------
# ------------------------------------------------------------------------------
best_balance <- final_model$best_balance
# best_balance <- list_method[["nnet"]][["best_balance"]]
# ------------------------------------------------------------------------------
# Importance -------------------------------------------------------------------
# ------------------------------------------------------------------------------
importance <- as.data.frame(t(final_model$importance))

names(importance) <- names(final_model[["data_scenario_list"]][[1]][["data_scenario"]])

# ------------------------------------------------------------------------------
# Performance ------------------------------------------------------------------
# ------------------------------------------------------------------------------
performance <- as.data.frame(t(final_model$performance_real_data))
# performance <- as.data.frame(t(list_method[["nnet"]][["performance_real_data"]]))
# # ------------------------------------------------------------------------------
# # Betas ------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# betas <- final_model[["data_scenario_list"]][[3]][["betas"]]

# ------------------------------------------------------------------------------
# Metrics ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
metrics <- final_model$metrics_list
# metrics <- list_method[["nnet"]][["metrics_list"]]
# ------------------------------------------------------------------------------
# Export to Excel --------------------------------------------------------------
# ------------------------------------------------------------------------------
# export <- list(best_balance, importance, performance, metrics)

path <- paste0("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/", name_data, "_metrics", ".xlsx")

write_xlsx(metrics, path = path)


load("~/PEAX/articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/betas_NN.Rdata")

for(i in 1:length(data_scenario_list)) {
  export <- data_scenario_list[[i]][["betas"]]
  path <- paste0("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/betas",i, ".xlsx")
  
  write_xlsx(export, path = path)
}

