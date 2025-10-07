# ============================================================================ #
# ========================= Betas comparation ================================ #
# ============================================================================ #


# ------------------------------------------------------------------------------
# Load libraries ---------------------------------------------------------------
#-------------------------------------------------------------------------------
library(writexl)

# ------------------------------------------------------------------------------
# Get names --------------------------------------------------------------------
#-------------------------------------------------------------------------------
list_data <- list.files(
  path = "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez",
  pattern = "\\.[Rr][Dd]ata$"
)

files <- sub("^result_(.*)\\.[Rr][Dd]ata$", "\\1", list_data)

files <- files[5:length(files)]

for (file_i in files) {
  
  path <- paste0("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_", file_i, ".Rdata")
  load(path)
  
  # if (file_i == "NN_SA") {
  #   final_model <- list_method$nnet
  #   save(final_model, file = "result_NN_SA.Rdata")
  # }
  
  names(final_model[["data_scenario_list"]]) <- c(0.75, 0.85, 0.95)
  
  scenarios <- c(0.75, 0.85, 0.95)
  
  for (scenarios_i in scenarios) {
    
    # get df
    df <- as.data.frame(final_model[["data_scenario_list"]][[as.character(scenarios_i)]][["betas"]][["beta"]])
    
    names(df) <- "beta"
    
    # create name
    obj_name <- sprintf("%s_%s", file_i, as.character(scenarios_i))
    
    # assing
    assign(obj_name, df, envir = .GlobalEnv)
    
  }

}

load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_g1.Rdata")
load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_g_mean.Rdata")
load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_g_x0_y0.Rdata")
# load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_SVM_SA.Rdata")
# load("articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_NN_SA.RData")

cor(
  x = as.data.frame(NN_g_x0_y0_0.75),
  y = betas_g_mean,
  method = "spearman"
)


# ------------------------------------------------------------------------------
# Correlation ------------------------------------------------------------------
# ------------------------------------------------------------------------------

## -- 1) Quedarme solo con data.frames útiles --
nms   <- ls(envir = .GlobalEnv)

is_df <- vapply(nms, function(x) inherits(get(x, .GlobalEnv), "data.frame"), logical(1))
keep_df <- nms[ is_df & !(nms %in% c("df","final_model")) ]

## (opcional) borrar todo lo demás:
# rm(list = setdiff(nms, keep_df), envir = .GlobalEnv); gc()

## -- 2) Detectar válidos y escenarios entre los data.frames que conservo --
# patrón: <modelo>_(g1|g_mean)_<escenario>
pat <- "^([A-Za-z]+)_(g1|g_mean|g_x0_y0|PI|SHAP|SA)_([A-Za-z0-9._]+)$"

objs <- grep(pat, keep_df, value = TRUE, perl = TRUE)

escenarios <- sort(unique(sub(pat, "\\3", objs)))

## --- 2) Función para construir betas_<escenario> automáticamente ---
make_betas_df <- function(esc) {
  
  suf <- paste0("_", gsub("\\.", "\\\\.", esc), "$")
  sel <- grep(suf, objs, value = TRUE)
  # if (length(sel) == 0) return(NULL)
  
  # nombres de columnas sin el sufijo de escenario
  col_names <- sub(suf, "", sel)
  
  # extraer vectores 'beta' (si el objeto es df y tiene columna beta)
  betas_list <- lapply(sel, function(nm) {
    x <- get(nm, envir = .GlobalEnv)
    if (is.data.frame(x) && "beta" %in% names(x)) return(as.numeric(x$beta))
    # si no cumple, devolvemos NULL y luego lo descartamos
    NULL
  })
  
  keep <- !sapply(betas_list, is.null)
  betas_list <- betas_list[keep]
  col_names  <- col_names[keep]
  
  as.data.frame(setNames(betas_list, col_names))
}

## --- 3) Construir betas_<escenario> y calcular correlaciones ---
cor_pearson  <- list()
cor_spearman <- list()
cor_kendall  <- list()

for (esc in escenarios) {
  
  B <- make_betas_df(esc)
  
  B <- cbind(B, betas_g1, betas_g_mean, betas_g_x0_y0)
  
  # crea objetos como tu idea: betas_0.75, betas_0.85, ...
  assign(paste0("betas_", gsub("\\.", "", esc)), B, envir = .GlobalEnv)
  
  # matrices de correlación para “todos con todos”
  cor_pearson[[esc]]  <- cor(B, method = "pearson",  use = "pairwise.complete.obs")
  cor_spearman[[esc]] <- cor(B, method = "spearman", use = "pairwise.complete.obs")
  cor_kendall[[esc]]  <- cor(B, method = "kendall",  use = "pairwise.complete.obs")
}



## 1) helper para dejar “bonita” la matriz
pretty_mat <- function(M) {
  if (is.null(M)) return(NULL)
  M <- as.matrix(M)
  if (nrow(M) == 0 || ncol(M) == 0) return(NULL)
  diag(M) <- 1
  # M <- round(M, 3)
  data.frame(Method = rownames(M), M, check.names = FALSE)
}

## 2) (opcional) si quieres crear objetos cor_pearson_075, etc.
for (esc in names(cor_pearson)) {
  assign(paste0("cor_pearson_", gsub("\\.", "", esc)),  cor_pearson[[esc]],  .GlobalEnv)
}
for (esc in names(cor_spearman)) {
  assign(paste0("cor_spearman_", gsub("\\.", "", esc)), cor_spearman[[esc]], .GlobalEnv)
}
for (esc in names(cor_kendall)) {
  assign(paste0("cor_kendall_", gsub("\\.", "", esc)),  cor_kendall[[esc]],  .GlobalEnv)
}

## 3) Exportar DIRECTAMENTE desde las listas (recomendado)
export_list <- list()

# Pearson
for (esc in names(cor_pearson)) {
  df <- pretty_mat(cor_pearson[[esc]])
  if (!is.null(df)) export_list[[paste0("pearson_", gsub("\\.", "", esc))]] <- df
}

# Spearman
for (esc in names(cor_spearman)) {
  df <- pretty_mat(cor_spearman[[esc]])
  if (!is.null(df)) export_list[[paste0("spearman_", gsub("\\.", "", esc))]] <- df
}

# Kendall
for (esc in names(cor_kendall)) {
  df <- pretty_mat(cor_kendall[[esc]])
  if (!is.null(df)) export_list[[paste0("kendall_", gsub("\\.", "", esc))]] <- df
}

# Si por algún motivo no hay nada, avisa:
if (length(export_list) == 0) {
  stop("No hay matrices de correlación para exportar. Revisa que betas_<esc> se hayan construido con ≥2 columnas.")
}

# 4) Escribir Excel (cada elemento = una hoja)
write_xlsx(export_list, path = "correlaciones2.xlsx")

# ------------------------------------------------------------------------------
# Projections ------------------------------------------------------------------
# ------------------------------------------------------------------------------

