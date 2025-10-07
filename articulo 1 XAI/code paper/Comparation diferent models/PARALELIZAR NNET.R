#########################################
# NNET evaluations (paralelo por tareas)#
#########################################

# --- Paquetes ---
devtools::load_all()   # para sesión principal (opcional si ya lo tienes)
library(caret)
library(parallel)
library(doParallel)
library(foreach)

# --- Config ---
technique <- "NNET"
scenario  <- 1:8
cut_off   <- c(0.55, 0.65, 0.75, 0.85, 0.95)

m         <- 10              # repeticiones por (escenario, umbral)
base_seed <- 1               # misma semilla “primera” para cada umbral

# --- Datos ---
load("articulo 1 XAI/code paper/Comparation diferent models/save_datasets_comparation.Rdata")
n <- nrow(save_data[[2]])
x <- 1; y <- 2
direction <- c(0, 1)

sc_name <- function(i){
  if (i == 1) "original" else if (i == 2) "0.2" else if (i == 3) "0.25" else if (i == 4) "0.3" else if (i == 5) "0.35" else if (i == 6) "0.4" else if (i == 7) "0.45" else if (i == 8) "0.5" else stop("scenario_i debe ser 1..8")
}

# Plantilla de grid y (¡ojo!) desactivamos paralelismo interno de caret
grid <- expand.grid(
  size  = c(1, 5, 10, 20, 30),
  decay = c(0.1, 0.01, 0.001, 0.001)
)

# --- Cluster paralelo ---
n_cores <- max(1, parallel::detectCores(logical = TRUE) - 1)
cl <- parallel::makeCluster(n_cores, type = "PSOCK")
on.exit(parallel::stopCluster(cl), add = TRUE)

# Ruta absoluta al proyecto (donde está DESCRIPTION y carpeta R/)
proj_dir <- normalizePath(".", mustWork = TRUE)

# Cargar dependencias + tu código en CADA worker (Opción B)
parallel::clusterCall(cl, function(path){
  suppressPackageStartupMessages({
    library(caret)
    library(nnet)  # método de caret
  })
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(path, quiet = TRUE)
  } else if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(path, quiet = TRUE)
  } else {
    stop("Instala 'devtools' o 'pkgload' para usar load_all().")
  }
  if (!exists("compute_target", mode = "function")) {
    stop("compute_target() no está disponible en el worker.")
  }
  NULL
}, proj_dir)

# Registrar backend para foreach (una vez)
doParallel::registerDoParallel(cl)

# Variables “globales” que usaremos en los workers (se exportan una vez por escenario)
common_vars <- c("x","y","direction","cut_off","grid","m","base_seed","n")

# --- Loop por escenarios (secuencial para no duplicar memoria masiva) ---
for (scenario_i in scenario) {
  
  # dataset según desbalanceo
  data <- if (scenario_i == 1) {
    save_data[[2]]
  } else if (scenario_i == 2) {
    save_data[[3]][["0.2"]]
  } else if (scenario_i == 3) {
    save_data[[3]][["0.25"]]
  } else if (scenario_i == 4) {
    save_data[[3]][["0.3"]]
  } else if (scenario_i == 5) {
    save_data[[3]][["0.35"]]
  } else if (scenario_i == 6) {
    save_data[[3]][["0.4"]]
  } else if (scenario_i == 7) {
    save_data[[3]][["0.45"]]
  } else if (scenario_i == 8) {
    save_data[[3]][["0.5"]]
  } else stop("scenario debe ser 1..8")
  
  data_original <- save_data[[2]]  # betas siempre sobre las unidades reales
  
  # Exportar objetos grandes/escenario-específicos una sola vez
  parallel::clusterExport(
    cl, varlist = c(common_vars, "data", "data_original"),
    envir = environment()
  )
  
  # Array para resultados
  betas_all <- array(NA_real_,
                     dim = c(n, length(cut_off), m),
                     dimnames = list(
                       NULL,
                       as.character(cut_off),
                       paste0("rep", seq_len(m))
                     ))
  seeds_used <- matrix(NA_integer_, nrow = length(cut_off), ncol = m,
                       dimnames = list(as.character(cut_off), paste0("rep", seq_len(m))))
  
  # --- Paralelizar sobre (umbral × repetición) ---
  res_list <- foreach(j = seq_along(cut_off), .combine = "c") %:%
    foreach(r = seq_len(m), .combine = "c") %dopar% {
      
      # Semilla reproducible: se reinicia por umbral; r=1 usa base_seed
      seed_r <- base_seed + r - 1
      set.seed(seed_r)
      
      # Definir MySummary aquí dentro (evita problemas de exportación)
      MySummary <- function (data, lev = NULL, model = NULL) {
        acc_kpp     <- defaultSummary(data, lev, model)
        auc_sen_spe <- twoClassSummary(data, lev, model)
        pre_rec     <- prSummary(data, lev, model)
        c(acc_kpp, auc_sen_spe, pre_rec)
      }
      
      trControl <- trainControl(
        method = "cv",
        number = 5,
        summaryFunction = MySummary,
        classProbs = TRUE,
        savePredictions = "all",
        allowParallel = FALSE   # <<< evita paralelismo interno de caret
      )
      
      # Entrenar
      model <- train(
        class_efficiency ~ .,
        data = data,
        method = "nnet",
        trControl = trControl,
        preProcess = c("center","scale"),
        tuneGrid = grid,
        metric = "Accuracy",
        trace = FALSE,
        maxit = 500,
        MaxNWts = 10000
      )
      
      # Calcular betas para este (umbral j, rep r)
      co <- cut_off[j]
      betas <- compute_target(
        data        = data_original[, c(x, y)],
        x           = x,
        y           = y,
        final_model = model,
        cut_off     = co,
        imp_vector  = direction
      )[["betas"]][, 1]
      
      # Devolver como lista nombrada para rearmar luego
      list(list(j = j, r = r, seed = seed_r, betas = betas))
    }
  
  # Reensamblar resultados en betas_all y seeds_used
  for (item in res_list) {
    j <- item$j; r <- item$r
    betas_all[, j, r] <- item$betas
    seeds_used[j, r]  <- item$seed
  }
  
  # Guardar
  # rute <- "articulo 1 XAI/code paper/Comparation diferent models/"
  rute <- "articulo 1 XAI/"
  name <- file.path(rute, paste0(technique, "_", sc_name(scenario_i), "_betas_m", m, "_PAR.Rdata"))
  save(betas_all, seeds_used, file = name)
}

# (El cluster se cerrará por el on.exit() definido arriba)
